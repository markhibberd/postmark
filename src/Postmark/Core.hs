{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (sendEmail) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
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
  ) responder

responder :: Response BL.ByteString -> PostmarkResponse
responder (Response (Status 200 _) _ _ body) =
  withJson 200 body (\(PostmarkResponseSuccessData ident at to) -> PostmarkResponseSuccess ident at to)
responder (Response (Status 401 _) _ _ _) =
  PostmarkResponseUnauthorized
responder (Response (Status 422 _) _ _ body) =
  withJson 422 body (\(PostmarkResponseErrorData code message) -> PostmarkResponseUnprocessible (toPostmarkError code) message)
responder (Response (Status 500 _) _ _ body) =
  PostmarkResponseServerError (toText body)
responder (Response (Status c _) _ _ body) =
  PostmarkResponseInvalidResponseCode c (toText body)

withJson :: FromJSON a => Int -> BL.ByteString -> (a -> PostmarkResponse) -> PostmarkResponse
withJson code bs f =
  let bt = toText bs
  in case parseJson bs of
    Left (Left msg) -> PostmarkResponseJsonSyntaxError code msg bt
    Left (Right msg) -> PostmarkResponseJsonFormatError code msg bt
    Right a -> f a

parseJson :: FromJSON a => BL.ByteString -> Either (Either Text Text) a
parseJson bs =
  case parseOnly json (toStrictBS bs) of
    Left msg -> Left (Left . pack $ msg)
    Right j -> case fromJSON j of
      (Error msg') -> Left (Right . pack $ msg')
      (Success a) -> Right a

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks
