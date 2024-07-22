module MindWM.Api.Components.Schemas.Vector2i where

import Autodocodec
import Autodocodec.OpenAPI()
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word32)
import GHC.Generics
import qualified Data.OpenApi as OpenAPI

data Vector2i = Vector2i {_x :: Word32, _y :: Word32}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenAPI.ToSchema)
    via (Autodocodec Vector2i)
instance HasCodec Vector2i where
  codec =
    object "Vector2i" $
      Vector2i
        <$> requiredField "x" "the X coordinate" .= _x
        <*> requiredField "y" "the Y coordinate" .= _x
