{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Core (
  email,
  emails,
  request
) where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Text.Encoding

import Network.Api.Support
import Network.Api.Postmark.Data
import Network.Api.Postmark.Request
import Network.Api.Postmark.Response
import Network.Api.Postmark.Settings
import Network.HTTP.Client.TLS
import Network.HTTP.Types

-- * Api endpoints


-- | Send a single email: http://developer.postmarkapp.com/developer-build.html
email :: Email -> PostmarkRequest' Sent
email e = PostmarkRequest POST "/email" $ setJson e

-- | Bulk send emails: http://developer.postmarkapp.com/developer-build.html#batching-messages
emails :: [Email] -> PostmarkRequest' [Sent]
emails es = PostmarkRequest POST "/email/batch" $ setJson es


-- * Run a request

-- | Run the specified request with the specified settings.
request :: PostmarkSettings -> PostmarkRequest e a -> IO (PostmarkResponse e a)
request settings (PostmarkRequest stdmethod url transform) =
  runRequest tlsManagerSettings stdmethod (apiUrl settings <> url) (
    addHeader ("Accept", "application/json") <>
    addHeader ("X-Postmark-Server-Token", encodeUtf8 $  apiToken settings) <>
    transform
  ) (basicResponder responder)


-- Low level error handling

responder :: (FromJSON e, FromJSON a) => Int -> BL.ByteString -> PostmarkResponse e a
responder 200 body = parseBodyWith body (syntaxErr 200 body) (formatErr 200 body) PostmarkSuccess
responder 401 _    = PostmarkUnauthorized
responder 422 body = parseBodyWith body (syntaxErr 422 body) (formatErr 422 body) PostmarkFailure
responder 500 body = PostmarkUnexpected ServerError 500 (Just . toText $ body) Nothing
responder c   body = PostmarkUnexpected UnexpectedResponseCode c (Just . toText $ body) Nothing
