{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module MindWM.Api where

import qualified Data.OpenApi as OpenAPI
import qualified Data.OpenApi.Declare as OpenAPI
import Data.Proxy ( Proxy(..) )
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import MindWM.Api.Components.Schemas.CloudEvent

-- uncurry4                        :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
-- uncurry4 f ~(a, b, c, d)        = f a b c d

components :: String
components =
  let 
    -- schema = T.unpack $ renderColouredSchemaViaCodec @CloudEvent
    -- oaSchema = TL.unpack $ TL.decodeUtf8 $ encodePretty $ OpenAPI.toSchema $ (Proxy :: Proxy CloudEvent)
    (definitions, _reference) = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy CloudEvent)) mempty
    json =
      JSON.object
        [ "components" JSON..= JSON.object
          [ "schemas" JSON..= definitions
          ]
        ]
    oaSchema = TL.unpack $ TL.decodeUtf8 $ encodePretty $ JSON.toJSON json
    -- oaSchema = OpenAPI.toSchema $ (Proxy :: Proxy Example)
  in oaSchema
  -- putStrLn schema
