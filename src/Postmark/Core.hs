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
    (Status 200 _) -> undefined -- FIX parse out success case
    (Status 401 _) -> UnauthorizedPostmarkResponse
    (Status 422 _) -> case parseOnly json (B.concat . BL.toChunks $ body) of
      Left _ -> undefined -- FIX handle malformed json
      Right j -> case fromJSON j of
        (Error _) -> undefined -- FIX handle non-spec json
        (Success (PostmarkResponseErrorData code message)) ->
          undefined -- FIX finish parsing out error code
--          UnprocessiblePostmarkResponse (toPostmarkError code) message
    (Status 500 _) -> ServerErrorPostmarkResponse b
    (Status c _) -> UnexpectedResponse c b
