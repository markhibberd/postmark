{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (sendEmail) where


import Control.Monad
--import Data.ByteString.Char8 (encodeUtf8, decodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
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

responder :: Response ByteString -> PostmarkResponse
responder (Response status _ _ body) =
  let b = LT.toStrict . LE.decodeUtf8 $ body
   in case status of
    status200 -> undefined -- FIX parse out success case
    status401 -> UnauthorizedPostmarkResponse
    status422 -> undefined -- FIX parse out client error
    status500 -> ServerErrorPostmarkResponse b
    (Status c _) -> UnexpectedResponse c b
