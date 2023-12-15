module Data.Yaml.Marked.Internal
  ( Warning (..)
  , decodeHelper
  , decodeAllHelper
  ) where

import Prelude

import Conduit
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.RWS.Strict (RWST, evalRWST)
import Control.Monad.Writer (MonadWriter (..), tell)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as M
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as Atto
import Data.Bifunctor (first, second)
import Data.Bitraversable (Bitraversable, bimapM)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Char (isOctDigit, ord, toUpper)
import Data.DList (DList)
import Data.Foldable (toList, traverse_)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as V
import Data.Yaml (ParseException (..))
import Data.Yaml.Marked
import Data.Yaml.Marked.Value hiding (Value (..))
import qualified Data.Yaml.Marked.Value as Marked
import Text.Libyaml hiding (decode, decodeFile, encode, encodeFile)
import qualified Text.Libyaml as Y
import UnliftIO.Exception

type MarkedValue = Marked Marked.Value

fromMarkedEvent :: MarkedEvent -> Marked Event
fromMarkedEvent MarkedEvent {..} =
  markedItem yamlEvent yamlStartMark yamlEndMark

markedValueFromEvent :: MarkedEvent -> a -> Marked a
markedValueFromEvent e a = a <$ fromMarkedEvent e

decodeHelper
  :: MonadIO m
  => (MarkedValue -> Either String a)
  -> ConduitT () MarkedEvent Parse ()
  -> m (Either ParseException (a, [Warning]))
decodeHelper = mkHelper parseOne

decodeAllHelper
  :: MonadIO m
  => (MarkedValue -> Either String a)
  -> ConduitT () MarkedEvent Parse ()
  -> m (Either ParseException ([a], [Warning]))
decodeAllHelper parseMarked = mkHelper parseAll $ traverse parseMarked

parseOne :: ConduitT MarkedEvent o Parse MarkedValue
parseOne = do
  docs <- parseAll
  case docs of
    [] -> pure $ markedZero Marked.Null
    [doc] -> pure doc
    _ -> throwIO MultipleDocuments

parseAll :: ConduitT MarkedEvent o Parse [MarkedValue]
parseAll =
  headC >>= \case
    Nothing -> pure []
    Just (MarkedEvent EventStreamStart _ _) -> parseDocs
    x -> missed x

parseDocs :: ConduitT MarkedEvent o Parse [MarkedValue]
parseDocs =
  headC >>= \case
    Just (MarkedEvent EventStreamEnd _ _) -> pure []
    Just (MarkedEvent EventDocumentStart _ _) -> do
      res <- parseO
      requireEvent EventDocumentEnd
      (res :) <$> parseDocs
    x -> missed x

missed :: MonadIO m => Maybe MarkedEvent -> m a
missed event = throwIO $ UnexpectedEvent (Y.yamlEvent <$> event) Nothing

mkHelper
  :: MonadIO m
  => ConduitT MarkedEvent Void Parse val
  -> (val -> Either String a)
  -> ConduitT () MarkedEvent Parse ()
  -> m (Either ParseException (a, [Warning]))
mkHelper eventParser f src = liftIO $ catches go handlers
 where
  go = first AesonException . firstM f <$> runParse (src .| eventParser)
  handlers =
    [ Handler $ \pe -> pure $ Left (pe :: ParseException)
    , Handler $ \ye -> pure $ Left $ InvalidYaml $ Just (ye :: YamlException)
    , Handler $ \ex -> throwIO @_ @SomeAsyncException ex
    , Handler $ \ex -> pure $ Left $ OtherParseException ex
    ]

newtype Warning = DuplicateKey JSONPath
  deriving stock (Eq, Show)

newtype ParseT m a = ParseT
  { unParseT :: RWST JSONPath (DList Warning) (Map String MarkedValue) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadResource
    , MonadReader JSONPath
    , MonadWriter (DList Warning)
    , MonadState (Map String MarkedValue)
    )

runParseT :: Monad m => ParseT m a -> m (a, [Warning])
runParseT p = second toList <$> evalRWST (unParseT p) [] Map.empty

type Parse = ParseT (ResourceT IO)

runParse :: ConduitT () Void Parse a -> IO (a, [Warning])
runParse = runResourceT . runParseT . runConduit

defineAnchor
  :: MonadState (Map String MarkedValue) m
  => MarkedValue
  -> String
  -> m ()
defineAnchor value name = modify $ Map.insert name value

lookupAnchor
  :: MonadState (Map String MarkedValue) m
  => String
  -> m (Maybe MarkedValue)
lookupAnchor = gets . Map.lookup

requireEvent :: MonadIO m => Event -> ConduitT MarkedEvent o m ()
requireEvent e = do
  f <- fmap Y.yamlEvent <$> headC
  unless (f == Just e) $ throwIO $ UnexpectedEvent f $ Just e

parseScalar
  :: MarkedEvent
  -> ByteString
  -> Anchor
  -> Style
  -> Tag
  -> ConduitT MarkedEvent o Parse Text
parseScalar e v a style tag = do
  let
    res = decodeUtf8With lenientDecode v
    anc = markedValueFromEvent e $ textToValue style tag res
  traverse_ (defineAnchor anc) a
  pure res

textToValue :: Style -> Tag -> Text -> Marked.Value
textToValue SingleQuoted _ t = Marked.String t
textToValue DoubleQuoted _ t = Marked.String t
textToValue _ StrTag t = Marked.String t
textToValue Folded _ t = Marked.String t
textToValue _ _ t
  | t `elem` ["null", "Null", "NULL", "~", ""] = Marked.Null
  | any (t `isLike`) ["y", "yes", "on", "true"] = Marked.Bool True
  | any (t `isLike`) ["n", "no", "off", "false"] = Marked.Bool False
  | Right x <- textToScientific t = Marked.Number x
  | otherwise = Marked.String t
 where
  x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
   where
    titleCased = toUpper (T.head ref) `T.cons` T.tail ref

textToScientific :: Text -> Either String Scientific
textToScientific = Atto.parseOnly (num <* Atto.endOfInput)
 where
  num =
    (fromInteger <$> ("0x" *> Atto.hexadecimal))
      <|> (fromInteger <$> ("0o" *> octal))
      <|> Atto.scientific

  octal = T.foldl' step 0 <$> Atto.takeWhile1 isOctDigit
   where
    step a c = (a `shiftL` 3) .|. fromIntegral (ord c - 48)

parseO :: ConduitT MarkedEvent o Parse MarkedValue
parseO = do
  me <- headC
  case me of
    Just e@(MarkedEvent (EventScalar v tag style a) _ _) ->
      markedValueFromEvent e
        . textToValue style tag
        <$> parseScalar e v a style tag
    Just (MarkedEvent (EventSequenceStart _ _ a) s _) ->
      parseS s 0 a id
    Just (MarkedEvent (EventMappingStart _ _ a) s _) ->
      parseM s mempty a M.empty
    Just (MarkedEvent (EventAlias an) _ _) -> do
      m <- lookupAnchor an
      case m of
        Nothing -> throwIO $ UnknownAlias an
        Just v -> pure v
    _ -> throwIO $ UnexpectedEvent (yamlEvent <$> me) Nothing

parseS
  :: YamlMark
  -> Int
  -> Anchor
  -> ([MarkedValue] -> [MarkedValue])
  -> ConduitT MarkedEvent o Parse MarkedValue
parseS startMark !n a front = do
  me <- peekC
  case me of
    Just (MarkedEvent EventSequenceEnd _ endMark) -> do
      dropC 1
      let res =
            markedItem
              (Marked.Array $ V.fromList $ front [])
              startMark
              endMark
      traverse_ (defineAnchor res) a
      pure res
    _ -> do
      o <- local (Index n :) parseO
      parseS startMark (succ n) a $ front . (:) o

parseM
  :: YamlMark
  -> Set Key
  -> Anchor
  -> KeyMap MarkedValue
  -> ConduitT MarkedEvent o Parse MarkedValue
parseM startMark mergedKeys a front = do
  me <- headC
  case me of
    Just (MarkedEvent EventMappingEnd _ endMark) -> do
      let res = markedItem (Marked.Object front) startMark endMark
      traverse_ (defineAnchor res) a
      pure res
    _ -> do
      s <- case me of
        Just e@(MarkedEvent (EventScalar v tag style a') _ _) ->
          fromText <$> parseScalar e v a' style tag
        Just (MarkedEvent (EventAlias an) _ _) -> do
          m <- lookupAnchor an
          case m of
            Nothing -> throwIO $ UnknownAlias an
            Just v | Marked.String t <- getMarkedItem v -> pure $ fromText t
            Just v -> throwIO $ NonStringKeyAlias an $ markedValueToValue v
        _ -> do
          path <- ask
          throwIO $ NonStringKey path

      (mergedKeys', al') <- local (Key s :) $ do
        o <- parseO
        let al = do
              when (M.member s front && Set.notMember s mergedKeys) $ do
                path <- asks reverse
                tell $ pure $ DuplicateKey path
              pure (Set.delete s mergedKeys, M.insert s o front)
        if s == "<<"
          then case getMarkedItem o of
            Marked.Object l -> pure (merge l)
            Marked.Array l -> pure $ merge $ List.foldl' mergeObjects M.empty $ V.toList l
            _ -> al
          else al
      parseM startMark mergedKeys' a al'
 where
  mergeObjects al v | Marked.Object om <- getMarkedItem v = M.union al om
  mergeObjects al _ = al

  merge xs = (Set.fromList (M.keys xs List.\\ M.keys front), M.union front xs)

firstM :: (Bitraversable t, Applicative f) => (a -> f a') -> t a b -> f (t a' b)
firstM f = bimapM f pure
