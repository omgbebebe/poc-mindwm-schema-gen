module MindWM.Api.Components.Schemas.EventPayload where

import Autodocodec
import Autodocodec.OpenAPI()
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Generics
import qualified Data.OpenApi as OpenAPI
import MindWM.Api.Components.Schemas.Clipboard (ClipboardType, ClipboardContext)
import MindWM.Api.Components.Schemas.Vector2i (Vector2i)
import Data.Void (Void)

data EventPayload
  = IoDocument
    { _input :: Text
    , _output :: Text
    , _ps1 :: Text
    }
  | IoClipboard
    { _selection_start :: Maybe Vector2i
    , _selection_end :: Maybe Vector2i
    , _clipboard_data :: Text
    , _clipboard_type :: !ClipboardType
    , _clipboard_context :: !ClipboardContext
    }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenAPI.ToSchema
    )
    via (Autodocodec EventPayload)

instance HasCodec EventPayload where
  codec = object "EventPayload" objectCodec

instance HasObjectCodec EventPayload where
  objectCodec = discriminatedUnionCodec "type" enc dec where
    iodocumentCodec :: JSONObjectCodec EventPayload
    iodocumentCodec = IoDocument
        <$> requiredField "input" "User input" .= _input
        <*> requiredField "output" "Command output (mix of stdout & stderr)" .= _output
        <*> requiredField "ps1" "ps1 (prompt) AFTER the input and output" .= _ps1

    clipboardCodec :: JSONObjectCodec EventPayload
    clipboardCodec = IoClipboard
        <$> optionalField "selection_start" "Start position of clipboard selection" .= _selection_start
        <*> optionalField "selection_end" "End position of clipboard selection" .= _selection_end
        <*> requiredField "data" "Clipboard content" .= _clipboard_data
        <*> requiredField "clipboard_type" "Type of clipboard" .= _clipboard_type
        <*> requiredField "clipboard_context" "Selection context" .= _clipboard_context
      
    enc :: EventPayload -> (Discriminator, ObjectCodec EventPayload ())
    enc = \case
      x@(IoDocument {}) -> ("iodocument", mapToEncoder x iodocumentCodec)
      x@(IoClipboard {}) -> ("clipboard", mapToEncoder x clipboardCodec)
    dec :: HashMap Discriminator (Text, ObjectCodec Void EventPayload)
    dec = HashMap.fromList
      [ ("iodocument", ("iodocument", mapToDecoder undefined iodocumentCodec))
      , ("clipboard", ("clipboard", mapToDecoder undefined clipboardCodec))
      ]
