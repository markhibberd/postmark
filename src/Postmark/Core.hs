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
  let b = LT.toStrict . LE.decodeUtf8 $ body
   in case status of
    -- FIX format date
    (Status 200 _) -> withJson body (\(PostmarkResponseSuccessData ident at to) -> PostmarkResponseSuccess ident undefined to)
    (Status 401 _) -> PostmarkResponseUnauthorized
    (Status 422 _) -> withJson body (\(PostmarkResponseErrorData code message) -> PostmarkResponseUnprocessible (toPostmarkError code) message)
    (Status 500 _) -> PostmarkResponseServerError b
    (Status c _) -> PostmarkResponseInvalidResponseCode c b

withJson :: FromJSON a => BL.ByteString -> (a -> PostmarkResponse) -> PostmarkResponse
withJson bs f =
  let bt = LT.toStrict . LE.decodeUtf8 $ bs
  in case parseJson bs of
    Left (Left msg) -> PostmarkResponseJsonSyntaxError 200 msg bt
    Left (Right msg) -> PostmarkResponseJsonFormatError 200 msg bt
    Right a -> f a

parseJson :: FromJSON a => BL.ByteString -> Either (Either Text Text) a
parseJson bs =
  let strict = B.concat . BL.toChunks $ bs
   in case parseOnly json strict of
      Left msg -> Left (Left . pack $ msg)
      Right j -> case fromJSON j of
        (Error msg') -> Left (Right . pack $ msg')
        (Success a) -> Right a
