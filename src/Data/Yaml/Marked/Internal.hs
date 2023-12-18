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
import Data.Char (isOctDigit, ord)
import Data.DList (DList)
import Data.Foldable (toList, traverse_)
import Data.List (foldl', (\\))
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
import UnliftIO.Exception

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
runParseT p = second toList <$> evalRWST (unParseT p) [] mempty

type Parse = ParseT (ResourceT IO)

runParse :: ConduitT () Void Parse a -> IO (a, [Warning])
runParse = runResourceT . runParseT . runConduit

defineAnchor
  :: MonadState (Map String (Marked Value)) m
  => String
  -> Marked Value
  -> m ()
defineAnchor name = modify . Map.insert name

lookupAnchor
  :: (MonadIO m, MonadState (Map String (Marked Value)) m)
  => String
  -> m (Marked Value)
lookupAnchor name =
  maybe (throwIO $ UnknownAlias name) pure =<< gets (Map.lookup name)

lookupAliasKey
  :: ( MonadIO m
     , MonadState (Map String (Marked Value)) m
     )
  => String
  -> m Key
lookupAliasKey an = do
  a <- lookupAnchor an
  case a of
    v | String t <- getMarkedItem v -> pure $ Key.fromText t
    v -> throwIO $ NonStringKeyAlias an $ valueToValue $ getMarkedItem v

decodeHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -- ^ Name for file being parsed
  -> ConduitT () MarkedEvent Parse ()
  -- ^ "Text.Libyaml" source
  -> m (Either ParseException (a, [Warning]))
decodeHelper parse fp src =
  mkHelper parseOne go $ src .| mapC (remarkEvents fp)
 where
  go = \case
    Nothing -> parse $ markedZero Null fp
    Just mv -> parse mv

decodeAllHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -- ^ Name for file being parsed
  -> ConduitT () MarkedEvent Parse ()
  -- ^ "Text.Libyaml" source
  -> m (Either ParseException ([a], [Warning]))
decodeAllHelper parse fp src =
  mkHelper parseAll (traverse parse) $ src .| mapC (remarkEvents fp)

remarkEvents :: FilePath -> MarkedEvent -> Marked Event
remarkEvents fp MarkedEvent {..} =
  markedItem yamlEvent fp yamlStartMark yamlEndMark

mkHelper
  :: MonadIO m
  => ConduitT (Marked Event) Void Parse val
  -> (val -> Either String a)
  -> ConduitT () (Marked Event) Parse ()
  -> m (Either ParseException (a, [Warning]))
mkHelper eventParser f src = liftIO $ catches go handlers
 where
  go = first AesonException . firstM f <$> runParse (src .| eventParser)
  handlers =
    [ Handler $ \pe -> pure $ Left pe
    , Handler $ \ye -> pure $ Left $ InvalidYaml $ Just ye
    , Handler $ \ex -> pure $ Left $ OtherParseException ex
    ]

throwUnexpectedEvent :: MonadIO m => Maybe (Marked Event) -> m a
throwUnexpectedEvent me =
  throwIO $ UnexpectedEvent (getMarkedItem <$> me) Nothing

requireEvent :: MonadIO m => Event -> ConduitT (Marked Event) o m ()
requireEvent e = do
  f <- fmap getMarkedItem <$> headC
  unless (f == Just e) $ throwIO $ UnexpectedEvent f $ Just e

parseOne :: ConduitT (Marked Event) o Parse (Maybe (Marked Value))
parseOne = do
  docs <- parseAll
  case docs of
    [] -> pure Nothing
    [doc] -> pure $ Just doc
    _ -> throwIO MultipleDocuments

parseAll :: ConduitT (Marked Event) o Parse [Marked Value]
parseAll =
  headC >>= \case
    Nothing -> pure []
    Just me | EventStreamStart <- getMarkedItem me -> parseStream
    x -> throwUnexpectedEvent x

parseStream :: ConduitT (Marked Event) o Parse [Marked Value]
parseStream =
  headC >>= \case
    Just me | EventStreamEnd <- getMarkedItem me -> pure []
    Just me | EventDocumentStart <- getMarkedItem me -> do
      res <- parseDocument
      requireEvent EventDocumentEnd
      (res :) <$> parseStream
    x -> throwUnexpectedEvent x

parseDocument :: ConduitT (Marked Event) o Parse (Marked Value)
parseDocument =
  headC >>= \case
    Just me
      | EventScalar v tag style a <- getMarkedItem me ->
          parseScalar me v tag style a
    Just me
      | EventSequenceStart _ _ a <- getMarkedItem me ->
          parseSequence (getMarkedStart me) 0 a id
    Just me
      | EventMappingStart _ _ a <- getMarkedItem me ->
          parseMapping (getMarkedStart me) mempty a mempty
    Just me | EventAlias an <- getMarkedItem me -> lookupAnchor an
    x -> throwIO $ UnexpectedEvent (getMarkedItem <$> x) Nothing

parseSequence
  :: YamlMark
  -> Int
  -> Anchor
  -> ([Marked Value] -> [Marked Value])
  -> ConduitT (Marked Event) o Parse (Marked Value)
parseSequence startMark !n a front =
  peekC >>= \case
    Just me | EventSequenceEnd <- getMarkedItem me -> do
      dropC 1
      let res =
            markedItem
              (Array $ V.fromList $ front [])
              (getMarkedPath me)
              startMark
              (getMarkedEnd me)
      res <$ traverse_ (`defineAnchor` res) a
    _ -> do
      o <- local (Index n :) parseDocument
      parseSequence startMark (succ n) a $ front . (:) o

parseMapping
  :: YamlMark
  -> Set Key
  -> Anchor
  -> KeyMap (Marked Value)
  -> ConduitT (Marked Event) o Parse (Marked Value)
parseMapping startMark mergedKeys a front =
  headC >>= \case
    Just me | EventMappingEnd <- getMarkedItem me -> do
      let res =
            markedItem
              (Object front)
              (getMarkedPath me)
              startMark
              (getMarkedEnd me)
      res <$ traverse_ (`defineAnchor` res) a
    me -> do
      s <- case me of
        Just me'
          | EventScalar v tag style a' <- getMarkedItem me' ->
              parseScalarKey me' v tag style a'
        Just me'
          | EventAlias an <- getMarkedItem me' ->
              lookupAliasKey an
        _ -> do
          path <- ask
          throwIO $ NonStringKey path

      (mergedKeys', al') <- local (Key s :) $ do
        o <- parseDocument

        let al = do
              when (KeyMap.member s front && Set.notMember s mergedKeys) $ do
                path <- asks reverse
                tell $ pure $ DuplicateKey path
              pure
                ( Set.delete s mergedKeys
                , KeyMap.insert s o front
                )

        if s == "<<"
          then case getMarkedItem o of
            Object l -> pure $ merge l
            Array l ->
              pure $
                merge $
                  foldl' mergeMarkedObject mempty $
                    toList l
            _ -> al
          else al

      parseMapping startMark mergedKeys' a al'
 where
  merge xs =
    ( Set.fromList (KeyMap.keys xs \\ KeyMap.keys front)
    , KeyMap.union front xs
    )

mergeMarkedObject
  :: KeyMap (Marked Value) -> Marked Value -> KeyMap (Marked Value)
mergeMarkedObject al v | Object om <- getMarkedItem v = KeyMap.union al om
mergeMarkedObject al _ = al

parseScalar
  :: Marked Event
  -> ByteString
  -> Tag
  -> Style
  -> Anchor
  -> ConduitT (Marked Event) o Parse (Marked Value)
parseScalar me v tag style a = do
  s <- parseScalarText me v tag style a
  pure $ textToValue style tag s <$ me

parseScalarKey
  :: Marked Event
  -> ByteString
  -> Tag
  -> Style
  -> Anchor
  -> ConduitT (Marked Event) o Parse Key
parseScalarKey me v tag style =
  fmap Key.fromText . parseScalarText me v tag style

parseScalarText
  :: Marked Event
  -> ByteString
  -> Tag
  -> Style
  -> Anchor
  -> ConduitT (Marked Event) o Parse Text
parseScalarText me v tag style = (res <$) . traverse_ (`defineAnchor` anc)
 where
  res = decodeUtf8With lenientDecode v
  anc = textToValue style tag res <$ me

textToValue :: Style -> Tag -> Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t = String t
textToValue Folded _ t = String t
textToValue _ _ t
  | t `isLike` "null" = Null
  | t `elem` ["~", ""] = Null
  | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
  | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
  | Right x <- textToScientific t = Number x
  | otherwise = String t
 where
  x `isLike` ref = x `elem` [ref, T.toUpper ref, T.toTitle ref]

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
