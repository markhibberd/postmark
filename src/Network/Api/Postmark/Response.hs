module Network.Api.Postmark.Response where

import qualified Data.ByteString.Lazy as BL
import Data.Text

import Network.Api.Postmark.Data (toText)
import Network.Api.Postmark.Error

data PostmarkResponse e a =
    PostmarkSuccess a
  | PostmarkUnauthorized
  | PostmarkFailure e
  | PostmarkUnexpected PostmarkUnexpectedType Int (Maybe Text) (Maybe Text)
  deriving (Eq, Show)

data PostmarkUnexpectedType =
    ServerError
  | UnexpectedResponseCode
  | JsonSyntaxError
  | JsonFormatError
  deriving (Eq, Show)

type PostmarkResponse' a =
  PostmarkResponse PostmarkError a



syntaxErr :: Int -> BL.ByteString -> Text -> PostmarkResponse e a
syntaxErr code body msg = PostmarkUnexpected JsonSyntaxError code (Just . toText $ body) (Just msg)

formatErr :: Int -> BL.ByteString -> Text -> PostmarkResponse e a
formatErr code body msg = PostmarkUnexpected JsonFormatError code (Just . toText $ body) (Just msg)
