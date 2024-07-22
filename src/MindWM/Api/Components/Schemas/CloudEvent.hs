module MindWM.Api.Components.Schemas.CloudEvent where

import Autodocodec
import Autodocodec.OpenAPI()
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.OpenApi as OpenAPI
import MindWM.Api.Components.Schemas.EventPayload (EventPayload)

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
