module AsyncApi.Spec.V3 where

import Data.Text (Text)

type VersionString = Text
type ContentType = Text

type Server = Text
type Channel = Text
type Operation = Text
type Component = Text
type ContactObject = Text
type LicenseObject = Text
type TagObject = Text
type ExternalDocumentationObject = Text

data Api = Api
  { _asyncapi           :: VersionString
  , _id                 :: Maybe Text
  , _info               :: InfoObject
  , _servers            :: [Server]
  , _defaultContentType :: Maybe ContentType
  , _channels           :: [Channel]
  , _operations         :: [Operation]
  , _components         :: [Component]
  }

data InfoObject = InfoObject
  { _title           :: Text
  , _version         :: Text
  , _description     :: Maybe Text
  , _termsOfService  :: Maybe Text
  , _contact         :: Maybe ContactObject
  , _license         :: Maybe LicenseObject
  , _tags            :: [TagObject]
  , _externalDocs    :: Maybe ExternalDocumentationObject
  }
