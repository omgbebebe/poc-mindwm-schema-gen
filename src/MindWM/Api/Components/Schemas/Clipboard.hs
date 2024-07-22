module MindWM.Api.Components.Schemas.Clipboard where

import Autodocodec
import Autodocodec.OpenAPI()
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word32)
import GHC.Generics
import qualified Data.OpenApi as OpenAPI

data ClipboardContext = ClipboardContext
  { _windowId :: Word32
  }
  deriving stock (Show, Eq, Generic)
  deriving ( FromJSON, ToJSON, OpenAPI.ToSchema ) via (Autodocodec ClipboardContext)

instance HasCodec ClipboardContext where
  codec = object "ClipboardContext" $ ClipboardContext
        <$> requiredField "windowId" "Xorg window_id" .= _windowId

data ClipboardType = Primary | Secondary | Clipboard 
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON,
      OpenAPI.ToSchema
    )
    via (Autodocodec ClipboardType)

instance HasCodec ClipboardType where
  codec = shownBoundedEnumCodec
