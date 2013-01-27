{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (
  sendEmail
) where

import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding

import Network.Api.Support
import Network.HTTP.Conduit
import Network.HTTP.Types
import Postmark.Data

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
responder 200 body = parseBodyWith body (syntaxErr 200 body) (decodeErr 200 body) successDataToResponse
responder 401 _    = PostmarkResponseUnauthorized
responder 422 body = parseBodyWith body (syntaxErr 200 body) (decodeErr 200 body) successDataToResponse
responder 500 body = PostmarkResponseServerError (toText body)
responder c   body = PostmarkResponseInvalidResponseCode c (toText body)
