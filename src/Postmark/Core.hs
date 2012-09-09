{-# LANGUAGE OverloadedStrings #-}
module Postmark.Core (sendEmail) where


import Data.ByteString.Lazy.Char8 hiding (unpack)
import Data.Aeson
import Data.Text
import Data.Text.Encoding

import Network.HTTP.Conduit
import Network.HTTP.Types (status200)

import Postmark.Data

-- FIX undodge
sendEmail :: PostmarkRequest Email -> IO PostmarkResponse
sendEmail req =
    parseUrl (unpack $ toUrl req "email") >>= \url ->
    return (url {
        method = "POST"
      , requestHeaders = [
          ("Accept", "application/json")
        , ("Content-Type", "application/json")
        , ("X-Postmark-Server-Token", encodeUtf8 $  postmarkToken req)
      ]
      , requestBody =  RequestBodyLBS . encode . toJSON $ postmarkEmail req
    }) >>= \r ->  withManager (httpLbs r) >>= return . responder

responder :: Response ByteString -> PostmarkResponse
responder res = undefined
--FIX handle each response code and parse into datatypes
