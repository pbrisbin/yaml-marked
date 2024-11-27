{-# LANGUAGE TupleSections #-}

module Data.Yaml.Marked.Internal
  ( Warning (..)
  , decodeHelper
  , decodeAllHelper

    -- * Debugging helpers
  , debugEventStream_
  , debugEventStream
  ) where

import Prelude

import Conduit
import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.RWS.Strict (RWST, evalRWST)
import Control.Monad.Writer (MonadWriter (..), tell)
import Data.Aeson.Compat.Key (Key)
import qualified Data.Aeson.Compat.Key as Key
import Data.Aeson.Compat.KeyMap (KeyMap)
import qualified Data.Aeson.Compat.KeyMap as KeyMap
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
import Data.Maybe (fromMaybe)
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
    v | String t <- markedItem v -> pure $ Key.fromText t
    v -> throwIO $ NonStringKeyAlias an $ valueToValue $ markedItem v

decodeHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -- ^ Name for file being parsed
  -> ConduitT () MarkedEvent Parse ()
  -- ^ "Text.Libyaml" source
  -> m (Either ParseException (a, [Warning]))
decodeHelper parse fp src =
  mkHelper parseOne go $ src .| mapC (`fromMarkedEvent` fp)
 where
  go = \case
    Nothing -> parse zeroMarkedNull
    Just mv -> parse mv

  zeroMarkedNull =
    Marked
      { markedItem = Null
      , markedPath = fp
      , markedJSONPath = Nothing
      , markedLocationStart = Location 0 0 0
      , markedLocationEnd = Location 0 0 0
      }

decodeAllHelper
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -- ^ Name for file being parsed
  -> ConduitT () MarkedEvent Parse ()
  -- ^ "Text.Libyaml" source
  -> m (Either ParseException ([a], [Warning]))
decodeAllHelper parse fp src =
  mkHelper parseAll (traverse parse) $ src .| mapC (`fromMarkedEvent` fp)

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
  throwIO $ UnexpectedEvent (markedItem <$> me) Nothing

requireEvent :: MonadIO m => Event -> ConduitT (Marked Event) o m ()
requireEvent e = do
  f <- fmap markedItem <$> headC
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
    Just me | EventStreamStart <- markedItem me -> parseStream
    x -> throwUnexpectedEvent x

parseStream :: ConduitT (Marked Event) o Parse [Marked Value]
parseStream =
  headC >>= \case
    Just me | EventStreamEnd <- markedItem me -> pure []
    Just me | EventDocumentStart <- markedItem me -> do
      res <- parseDocument
      requireEvent EventDocumentEnd
      (res :) <$> parseStream
    x -> throwUnexpectedEvent x

parseDocument :: ConduitT (Marked Event) o Parse (Marked Value)
parseDocument =
  headC >>= \case
    Just me
      | EventScalar v tag style a <- markedItem me ->
          parseScalar me v tag style a
    Just me
      | EventSequenceStart _ _ a <- markedItem me ->
          parseSequence (markedLocationStart me) Nothing 0 a id
    Just me
      | EventMappingStart _ _ a <- markedItem me ->
          parseMapping (markedLocationStart me) Nothing mempty a mempty
    Just me | EventAlias an <- markedItem me -> lookupAnchor an
    x -> throwIO $ UnexpectedEvent (markedItem <$> x) Nothing

parseSequence
  :: Location
  -- ^ Location where the sequence started
  -> Maybe Location
  -- ^ Ending location of last item within the sequence
  -> Int
  -> Anchor
  -> ([Marked Value] -> [Marked Value])
  -> ConduitT (Marked Event) o Parse (Marked Value)
parseSequence startLocation mEndLocation !n a front =
  peekC >>= \case
    Just me | EventSequenceEnd <- markedItem me -> do
      dropC 1
      path <- asks reverse
      let res =
            Marked
              { markedItem = Array $ V.fromList $ front []
              , markedPath = markedPath me
              , markedJSONPath = Just path
              , markedLocationStart = startLocation
              , markedLocationEnd = fromMaybe startLocation mEndLocation
              }
      res <$ traverse_ (`defineAnchor` res) a
    _ -> do
      o <- local (Index n :) parseDocument
      parseSequence startLocation (Just $ markedLocationEnd o) (succ n) a $
        front . (:) o

parseMapping
  :: Location
  -- ^ Location where the mapping started
  -> Maybe Location
  -- ^ Ending location of last item within the map
  -> Set Key
  -> Anchor
  -> KeyMap (Marked Value)
  -> ConduitT (Marked Event) o Parse (Marked Value)
parseMapping startLocation mEndLocation mergedKeys a front =
  headC >>= \case
    Just me | EventMappingEnd <- markedItem me -> do
      path <- asks reverse
      let res =
            Marked
              { markedItem = Object front
              , markedPath = markedPath me
              , markedJSONPath = Just path
              , markedLocationStart = startLocation
              , markedLocationEnd = fromMaybe startLocation mEndLocation
              }
      res <$ traverse_ (`defineAnchor` res) a
    me -> do
      s <- case me of
        Just me'
          | EventScalar v tag style a' <- markedItem me' ->
              parseScalarKey me' v tag style a'
        Just me'
          | EventAlias an <- markedItem me' ->
              lookupAliasKey an
        _ -> do
          path <- asks reverse
          throwIO $ NonStringKey path

      ((mergedKeys', al'), endLocation) <- local (Key s :) $ do
        o <- parseDocument

        fmap (,markedLocationEnd o) $ do
          let al = do
                when (KeyMap.member s front && Set.notMember s mergedKeys) $ do
                  path <- asks reverse
                  tell $ pure $ DuplicateKey path
                pure
                  ( Set.delete s mergedKeys
                  , KeyMap.insert s o front
                  )

          if s == "<<"
            then case markedItem o of
              Object l -> pure $ merge l
              Array l ->
                pure $
                  merge $
                    foldl' mergeMarkedObject mempty $
                      toList l
              _ -> al
            else al

      parseMapping startLocation (Just endLocation) mergedKeys' a al'
 where
  merge xs =
    ( Set.fromList (KeyMap.keys xs \\ KeyMap.keys front)
    , KeyMap.union front xs
    )

mergeMarkedObject
  :: KeyMap (Marked Value) -> Marked Value -> KeyMap (Marked Value)
mergeMarkedObject al v | Object om <- markedItem v = KeyMap.union al om
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
  let mv = textToValue style tag s <$ me
  path <- lift $ asks reverse
  pure $ mv {markedJSONPath = Just path}

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

debugEventStream_ :: ByteString -> IO ()
debugEventStream_ = void . debugEventStream

debugEventStream :: ByteString -> IO [MarkedEvent]
debugEventStream bs =
  runResourceT $
    runConduit $
      Y.decodeMarked bs
        .| iterMC (liftIO . printEvent)
        .| sinkList
 where
  showMark YamlMark {..} = show (yamlIndex, yamlLine, yamlColumn)

  printEvent MarkedEvent {..} = do
    putStrLn $
      mconcat
        [ "Event: " <> show yamlEvent
        , ", location: " <> showMark yamlStartMark
        , "-" <> showMark yamlEndMark
        ]
