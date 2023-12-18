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
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPath, JSONPathElement (..))
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
import Data.Yaml.Marked.Value
import Text.Libyaml hiding (decode, decodeFile, encode, encodeFile)
import qualified Text.Libyaml as Y
import UnliftIO.Exception

decodeHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> ConduitT () MarkedEvent Parse ()
  -> m (Either ParseException (a, [Warning]))
decodeHelper = mkHelper parseOne

decodeAllHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> ConduitT () MarkedEvent Parse ()
  -> m (Either ParseException ([a], [Warning]))
decodeAllHelper parseMarked = mkHelper parseAll $ traverse parseMarked

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
    [ Handler $ \pe -> pure $ Left pe
    , Handler $ \ye -> pure $ Left $ InvalidYaml $ Just ye
    , Handler $ \ex -> pure $ Left $ OtherParseException ex
    ]

throwUnexpectedEvent :: MonadIO m => Maybe MarkedEvent -> m a
throwUnexpectedEvent event = throwIO $ UnexpectedEvent (Y.yamlEvent <$> event) Nothing

requireEvent :: MonadIO m => Event -> ConduitT MarkedEvent o m ()
requireEvent e = do
  f <- fmap Y.yamlEvent <$> headC
  unless (f == Just e) $ throwIO $ UnexpectedEvent f $ Just e

parseOne :: ConduitT MarkedEvent o Parse (Marked Value)
parseOne = do
  docs <- parseAll
  case docs of
    [] -> pure $ markedZero Null
    [doc] -> pure doc
    _ -> throwIO MultipleDocuments

parseAll :: ConduitT MarkedEvent o Parse [Marked Value]
parseAll =
  headC >>= \case
    Nothing -> pure []
    Just (MarkedEvent EventStreamStart _ _) -> parseStream
    x -> throwUnexpectedEvent x

parseStream :: ConduitT MarkedEvent o Parse [Marked Value]
parseStream =
  headC >>= \case
    Just (MarkedEvent EventStreamEnd _ _) -> pure []
    Just (MarkedEvent EventDocumentStart _ _) -> do
      res <- parseDocument
      requireEvent EventDocumentEnd
      (res :) <$> parseStream
    x -> throwUnexpectedEvent x

newtype Warning = DuplicateKey JSONPath
  deriving stock (Eq, Show)

newtype ParseT m a = ParseT
  { unParseT :: RWST JSONPath (DList Warning) (Map String (Marked Value)) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadResource
    , MonadReader JSONPath
    , MonadWriter (DList Warning)
    , MonadState (Map String (Marked Value))
    )

runParseT :: Monad m => ParseT m a -> m (a, [Warning])
runParseT p = second toList <$> evalRWST (unParseT p) [] Map.empty

type Parse = ParseT (ResourceT IO)

runParse :: ConduitT () Void Parse a -> IO (a, [Warning])
runParse = runResourceT . runParseT . runConduit

defineAnchor
  :: MonadState (Map String (Marked Value)) m
  => Marked Value
  -> String
  -> m ()
defineAnchor value name = modify $ Map.insert name value

lookupAnchor
  :: MonadState (Map String (Marked Value)) m
  => String
  -> m (Maybe (Marked Value))
lookupAnchor = gets . Map.lookup

parseDocument :: ConduitT MarkedEvent o Parse (Marked Value)
parseDocument = do
  me <- headC
  case me of
    Just e@(MarkedEvent (EventScalar v tag style a) _ _) -> do
      mv <-
        textToValue style tag
          <$> parseScalar e v a style tag
      pure $ mv <$ markedEvent e
    Just (MarkedEvent (EventSequenceStart _ _ a) s _) ->
      parseS s 0 a id
    Just (MarkedEvent (EventMappingStart _ _ a) s _) ->
      parseM s mempty a KeyMap.empty
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
  -> ([Marked Value] -> [Marked Value])
  -> ConduitT MarkedEvent o Parse (Marked Value)
parseS startMark !n a front = do
  me <- peekC
  case me of
    Just (MarkedEvent EventSequenceEnd _ endMark) -> do
      dropC 1
      let res =
            markedItem
              (Array $ V.fromList $ front [])
              startMark
              endMark
      traverse_ (defineAnchor res) a
      pure res
    _ -> do
      o <- local (Index n :) parseDocument
      parseS startMark (succ n) a $ front . (:) o

parseM
  :: YamlMark
  -> Set Key
  -> Anchor
  -> KeyMap (Marked Value)
  -> ConduitT MarkedEvent o Parse (Marked Value)
parseM startMark mergedKeys a front = do
  me <- headC
  case me of
    Just (MarkedEvent EventMappingEnd _ endMark) -> do
      let res = markedItem (Object front) startMark endMark
      traverse_ (defineAnchor res) a
      pure res
    _ -> do
      s <- case me of
        Just e@(MarkedEvent (EventScalar v tag style a') _ _) ->
          Key.fromText <$> parseScalar e v a' style tag
        Just (MarkedEvent (EventAlias an) _ _) -> do
          m <- lookupAnchor an
          case m of
            Nothing -> throwIO $ UnknownAlias an
            Just v | String t <- getMarkedItem v -> pure $ Key.fromText t
            Just v ->
              throwIO $
                NonStringKeyAlias an $
                  valueToValue $
                    getMarkedItem v
        _ -> do
          path <- ask
          throwIO $ NonStringKey path

      (mergedKeys', al') <- local (Key s :) $ do
        o <- parseDocument
        let al = do
              when (KeyMap.member s front && Set.notMember s mergedKeys) $ do
                path <- asks reverse
                tell $ pure $ DuplicateKey path
              pure (Set.delete s mergedKeys, KeyMap.insert s o front)
        if s == "<<"
          then case getMarkedItem o of
            Object l -> pure (merge l)
            Array l -> pure $ merge $ List.foldl' mergeObjects KeyMap.empty $ V.toList l
            _ -> al
          else al
      parseM startMark mergedKeys' a al'
 where
  mergeObjects al v | Object om <- getMarkedItem v = KeyMap.union al om
  mergeObjects al _ = al

  merge xs =
    (Set.fromList (KeyMap.keys xs List.\\ KeyMap.keys front), KeyMap.union front xs)

parseScalar
  :: MarkedEvent
  -> ByteString
  -> Anchor
  -> Style
  -> Tag
  -> ConduitT MarkedEvent o Parse Text
parseScalar e v a style tag = res <$ traverse_ (defineAnchor anc) a
 where
  res = decodeUtf8With lenientDecode v
  anc = textToValue style tag res <$ markedEvent e

textToValue :: Style -> Tag -> Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t = String t
textToValue Folded _ t = String t
textToValue _ _ t
  | t `elem` ["null", "Null", "NULL", "~", ""] = Null
  | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
  | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
  | Right x <- textToScientific t = Number x
  | otherwise = String t
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

firstM :: (Bitraversable t, Applicative f) => (a -> f a') -> t a b -> f (t a' b)
firstM f = bimapM f pure
