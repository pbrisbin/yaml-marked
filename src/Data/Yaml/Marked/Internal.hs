module Data.Yaml.Marked.Internal
  ( Warning (..)
  , decodeHelper
  , decodeHelper_
  , decodeAllHelper
  , decodeAllHelper_
  ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as M
import Data.Aeson.Types hiding (parse)
import qualified Data.Attoparsec.Text as Atto
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Char (isOctDigit, ord, toUpper)
import Data.Conduit (ConduitM, runConduit, (.|))
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
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
import Data.Void (Void)
import Data.Yaml (ParseException (..))
import Data.Yaml.Marked
import Data.Yaml.Marked.Value hiding (Value (..))
import qualified Data.Yaml.Marked.Value as Marked
import Text.Libyaml hiding (decode, decodeFile, encode, encodeFile)
import qualified Text.Libyaml as Y

type MarkedValue = Marked Marked.Value

fromMarkedEvent :: MarkedEvent -> Marked Event
fromMarkedEvent MarkedEvent {..} =
  markedItem yamlEvent yamlStartMark yamlEndMark

markedValueFromEvent :: MarkedEvent -> a -> Marked a
markedValueFromEvent e a = a <$ fromMarkedEvent e

decodeHelper
  :: (MarkedValue -> Either String a)
  -> ConduitM () MarkedEvent Parse ()
  -> IO (Either ParseException ([Warning], Either String a))
decodeHelper parseMarked = mkHelper parse throwIO $ \(v, st) ->
  Right (parseStateWarnings st, parseMarked v)

decodeHelper_
  :: (MarkedValue -> Either String a)
  -> ConduitM () MarkedEvent Parse ()
  -> IO (Either ParseException ([Warning], a))
decodeHelper_ parseMarked = mkHelper parse catchLeft $ \(v, st) ->
  case parseMarked v of
    Left e -> Left $ AesonException e
    Right x -> Right (parseStateWarnings st, x)

decodeAllHelper
  :: (MarkedValue -> Either String a)
  -> ConduitM () MarkedEvent Parse ()
  -> IO (Either ParseException ([Warning], Either String [a]))
decodeAllHelper parseMarked = mkHelper parseAll throwIO $ \(vs, st) ->
  Right (parseStateWarnings st, mapM parseMarked vs)

decodeAllHelper_
  :: (MarkedValue -> Either String a)
  -> ConduitM () MarkedEvent Parse ()
  -> IO (Either ParseException ([Warning], [a]))
decodeAllHelper_ parseMarked = mkHelper parseAll catchLeft $ \(vs, st) ->
  case mapM parseMarked vs of
    Left e -> Left $ AesonException e
    Right xs -> Right (parseStateWarnings st, xs)

parse :: ReaderT JSONPath (ConduitM MarkedEvent o Parse) MarkedValue
parse = do
  docs <- parseAll
  case docs of
    [] -> pure $ markedZero Marked.Null
    [doc] -> pure doc
    _ -> liftIO $ throwIO MultipleDocuments

parseAll :: ReaderT JSONPath (ConduitM MarkedEvent o Parse) [MarkedValue]
parseAll = do
  streamStart <- lift CL.head
  case streamStart of
    Nothing ->
      -- empty string input
      pure []
    Just (MarkedEvent EventStreamStart _ _) ->
      -- empty file input, comment only string/file input
      parseDocs
    _ -> missed streamStart
 where
  parseDocs = do
    documentStart <- lift CL.head
    case documentStart of
      Just (MarkedEvent EventStreamEnd _ _) -> pure []
      Just (MarkedEvent EventDocumentStart _ _) -> do
        res <- parseO
        requireEvent EventDocumentEnd
        (res :) <$> parseDocs
      _ -> missed documentStart
  missed event = liftIO $ throwIO $ UnexpectedEvent (Y.yamlEvent <$> event) Nothing

catchLeft :: SomeException -> IO (Either ParseException a)
catchLeft = pure . Left . OtherParseException

mkHelper
  :: ReaderT JSONPath (ConduitM MarkedEvent Void Parse) val
  -- ^ parse libyaml events as MarkedValue or [MarkedValue]
  -> (SomeException -> IO (Either ParseException a))
  -- ^ what to do with unhandled exceptions
  -> ((val, ParseState) -> Either ParseException a)
  -- ^ further transform and parse results
  -> ConduitM () MarkedEvent Parse ()
  -- ^ the libyaml event (string/file) source
  -> IO (Either ParseException a)
mkHelper eventParser onOtherExc extractResults src =
  catches
    (extractResults <$> parseSrc eventParser src)
    [ Handler $ \pe -> pure $ Left (pe :: ParseException)
    , Handler $ \ye -> pure $ Left $ InvalidYaml $ Just (ye :: YamlException)
    , Handler $ \sae -> throwIO (sae :: SomeAsyncException)
    , Handler onOtherExc
    ]

data ParseState = ParseState
  { parseStateAnchors :: Map String MarkedValue
  , parseStateWarnings :: [Warning]
  }

type Parse = StateT ParseState (ResourceT IO)

defineAnchor
  :: MarkedValue -> String -> ReaderT JSONPath (ConduitM e o Parse) ()
defineAnchor value name = modify (modifyAnchors $ Map.insert name value)

modifyAnchors
  :: (Map String MarkedValue -> Map String MarkedValue) -> ParseState -> ParseState
modifyAnchors f st = st {parseStateAnchors = f (parseStateAnchors st)}

lookupAnchor
  :: String -> ReaderT JSONPath (ConduitM e o Parse) (Maybe MarkedValue)
lookupAnchor name = gets (Map.lookup name . parseStateAnchors)

newtype Warning = DuplicateKey JSONPath
  deriving stock (Eq, Show)

addWarning :: Warning -> ReaderT JSONPath (ConduitM e o Parse) ()
addWarning w = modify (modifyWarnings (w :))
 where
  modifyWarnings :: ([Warning] -> [Warning]) -> ParseState -> ParseState
  modifyWarnings f st = st {parseStateWarnings = f (parseStateWarnings st)}

requireEvent :: Event -> ReaderT JSONPath (ConduitM MarkedEvent o Parse) ()
requireEvent e = do
  f <- lift $ fmap Y.yamlEvent <$> CL.head
  unless (f == Just e) $
    liftIO $
      throwIO $
        UnexpectedEvent f $
          Just e

parseSrc
  :: ReaderT JSONPath (ConduitM MarkedEvent Void Parse) val
  -> ConduitM () MarkedEvent Parse ()
  -> IO (val, ParseState)
parseSrc eventParser src =
  runResourceT $
    runStateT
      (runConduit $ src .| runReaderT eventParser [])
      (ParseState Map.empty [])

parseScalar
  :: MarkedEvent
  -> ByteString
  -> Anchor
  -> Style
  -> Tag
  -> ReaderT JSONPath (ConduitM MarkedEvent o Parse) Text
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

parseO :: ReaderT JSONPath (ConduitM MarkedEvent o Parse) MarkedValue
parseO = do
  me <- lift CL.head
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
        Nothing -> liftIO $ throwIO $ UnknownAlias an
        Just v -> pure v
    _ -> liftIO $ throwIO $ UnexpectedEvent (yamlEvent <$> me) Nothing

parseS
  :: YamlMark
  -> Int
  -> Anchor
  -> ([MarkedValue] -> [MarkedValue])
  -> ReaderT JSONPath (ConduitM MarkedEvent o Parse) MarkedValue
parseS startMark !n a front = do
  me <- lift CL.peek
  case me of
    Just (MarkedEvent EventSequenceEnd _ endMark) -> do
      lift $ CL.drop 1
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
  -> ReaderT JSONPath (ConduitM MarkedEvent o Parse) MarkedValue
parseM startMark mergedKeys a front = do
  me <- lift CL.head
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
            Nothing -> liftIO $ throwIO $ UnknownAlias an
            Just v | Marked.String t <- getMarkedItem v -> pure $ fromText t
            Just v -> liftIO $ throwIO $ NonStringKeyAlias an $ markedValueToValue v
        _ -> do
          path <- ask
          liftIO $ throwIO $ NonStringKey path

      (mergedKeys', al') <- local (Key s :) $ do
        o <- parseO
        let al = do
              when (M.member s front && Set.notMember s mergedKeys) $ do
                path <- asks reverse
                addWarning (DuplicateKey path)
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
