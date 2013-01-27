{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (sendEmail) where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Network.Api.Support
import Network.HTTP.Conduit
import Network.HTTP.Types
import Postmark.Data

-- FIX split into Postmark.Network
-- FIX add sendBulkEmail

sendEmail :: PostmarkRequest Email -> IO PostmarkResponse
sendEmail req =
  runRequest def POST (toUrl req "email") (
    setHeaders [
        ("Accept", "application/json")
      , ("Content-Type", "application/json")
      , ("X-Postmark-Server-Token", encodeUtf8 $  postmarkToken req)
      ] <>
    setJson (postmarkEmail req)
  ) (basicResponder responder)

responder :: Int -> BL.ByteString -> PostmarkResponse
responder 200 body =
  withJson 200 body (\(PostmarkResponseSuccessData ident at to) -> PostmarkResponseSuccess ident at to)
responder 401 _ =
  PostmarkResponseUnauthorized
responder 422 body =
  withJson 422 body (\(PostmarkResponseErrorData code message) -> PostmarkResponseUnprocessible (toPostmarkError code) message)
responder 500 body =
  PostmarkResponseServerError (toText body)
responder c body =
  PostmarkResponseInvalidResponseCode c (toText body)

withJson :: FromJSON a => Int -> BL.ByteString -> (a -> PostmarkResponse) -> PostmarkResponse
withJson code body f =
  parseBodyWith body (\msg -> PostmarkResponseJsonSyntaxError code msg (toText body)) (\msg -> PostmarkResponseJsonFormatError code msg (toText body)) f

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8
