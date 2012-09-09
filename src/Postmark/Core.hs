{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (sendEmail) where


import Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Network.HTTP.Conduit
import Network.HTTP.Types

import Postmark.Data

-- FIX undodge
sendEmail :: PostmarkRequest Email -> IO PostmarkResponse
sendEmail req =
    parseUrl (unpack $ toUrl req "email") >>= \url ->
    (liftM responder . withManager . httpLbs) (url {
        method = "POST"
      , requestHeaders = [
          ("Accept", "application/json")
        , ("Content-Type", "application/json")
        , ("X-Postmark-Server-Token", encodeUtf8 $  postmarkToken req)
      ]
      , requestBody =  RequestBodyLBS . encode . toJSON $ postmarkEmail req
      })

responder :: Response BL.ByteString -> PostmarkResponse
responder (Response status _ _ body) =
  let bt = LT.toStrict . LE.decodeUtf8 $ body
   in case status of
    -- FIX format date
    (Status 200 _) -> withJson 200 body (\(PostmarkResponseSuccessData ident at to) -> PostmarkResponseSuccess ident undefined to)
    (Status 401 _) -> PostmarkResponseUnauthorized
    (Status 422 _) -> withJson 422 body (\(PostmarkResponseErrorData code message) -> PostmarkResponseUnprocessible (toPostmarkError code) message)
    (Status 500 _) -> PostmarkResponseServerError bt
    (Status c _) -> PostmarkResponseInvalidResponseCode c bt

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
