{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
module MindWM.Api where


import Autodocodec
import Autodocodec.Yaml
import Autodocodec.OpenAPI
import qualified Data.OpenApi as OpenAPI
import qualified Data.OpenApi.Declare as OpenAPI
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Void (Void)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.IO as T
import Data.List.NonEmpty (NonEmpty)
import Data.Tuple.Extra

data EventPayload
  = IoDocument
    { _input :: Text
    , _output :: Text
    , _ps1 :: Text
    }
  | Clipboard
    { _clip_data :: Text
    , _context :: Text
    }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenAPI.ToSchema
    )
    via (Autodocodec EventPayload)

instance HasCodec EventPayload where
  codec = object "EventPayload" $ objectCodec

instance HasObjectCodec EventPayload where
  objectCodec = discriminatedUnionCodec "type" enc dec where
    iodocumentCodec :: JSONObjectCodec (Text, Text, Text)
    iodocumentCodec = (,,)
        <$> requiredField "input" "User input" .= fst3
        <*> requiredField "output" "Command output (mix of stdout & stderr)" .= snd3
        <*> requiredField "ps1" "ps1 (prompt) AFTER the input and output" .= thd3
        
      
    clipboardCodec :: JSONObjectCodec (Text, Text)
    clipboardCodec = (,)
        <$> requiredField "data" "Clipboard data" .= fst
        <*> requiredField "context" "Selection context" .= snd
      
    enc :: EventPayload -> (Discriminator, ObjectCodec EventPayload ())
    enc = \case
      IoDocument i o p -> ("iodocument", mapToEncoder (i, o, p) iodocumentCodec)
      Clipboard t c -> ("clipboard", mapToEncoder (t, c) clipboardCodec)
    dec :: HashMap Discriminator (Text, ObjectCodec Void EventPayload)
    dec = HashMap.fromList
      [ ("iodocument", ("iodocument", mapToDecoder (uncurry3 IoDocument) iodocumentCodec))
      , ("clipboard", ("clipboard", mapToDecoder (uncurry Clipboard) clipboardCodec))
      ]

data CloudEvent = CloudEvent
  { _id :: Text
  , _source :: Text
  , _specversion :: Text
  , _type :: Text
  , _datacontenttype :: Maybe Text
  , _dataschema :: Maybe Text
  , _subject :: Maybe Text
  , _time :: Maybe Text
  , _data :: EventPayload
  , _data_base64 :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenAPI.ToSchema
    )
    via (Autodocodec CloudEvent)

instance HasCodec CloudEvent where
  codec =
    object "CloudEvent" $
      CloudEvent
        <$> requiredField "id" "Identifies the event" .= _id
        <*> requiredField "source" "Identifies the context in which an event happened" .= _source
        <*> requiredField "specversion" "The version of the CloudEvents specification which the event uses" .= _specversion
        <*> requiredField "type" "Describes the type of event related to the originating occurrence" .= _type
        <*> optionalField "datacontenttype" "Content type of the data value. Must adhere to RFC 2046 format" .= _datacontenttype
        <*> optionalField "dataschema" "Identifies the schema that data adheres to" .= _dataschema
        <*> optionalField "subject" "Describes the subject of the event in the context of the event producer (identified by source)" .= _subject
        <*> optionalField "time" "Timestamp of when the occurrence happened. Must adhere to RFC 3339" .= _time
        <*> requiredField "data" "The event payload" .= _data
        <*> optionalField "data_base64" "Base64 encoded event payload. Must adhere to RFC4648" .= _data_base64


fun :: IO ()
fun =  do
  let schema = T.unpack $ renderColouredSchemaViaCodec @CloudEvent
      --oaSchema = TL.unpack $ TL.decodeUtf8 $ encodePretty $ OpenAPI.toSchema $ (Proxy :: Proxy CloudEvent)
      (definitions, reference) = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy CloudEvent)) mempty
      json =
        JSON.object
              [ "definitions" JSON..= definitions,
                "reference" JSON..= reference
              ]
      oaSchema = TL.unpack $ TL.decodeUtf8 $ encodePretty $ JSON.toJSON json
      --oaSchema = OpenAPI.toSchema $ (Proxy :: Proxy Example)
  putStrLn oaSchema
--  putStrLn schema
