module Postmark.Core where

{-
postmarkrequest :: OutboundEmail -> ZZ (Request m)
postmarkrequest email = liftIO $
  parseUrl "http://api.postmarkapp.com/email" >>= \url ->
    return $ url {
        method = "POST"
      , requestHeaders = [
          ("Accept", "application/json")
        , ("Content-Type", "application/json")
        , ("X-Postmark-Server-Token", "95594c9d-fbc4-4599-b7bc-2f9cbb8f53c7")
      ]
      , requestBody =  RequestBodyLBS . encode . toJSON $ email
    }

postmark :: OutboundEmail -> ZZ ()
postmark email =
  postmarkrequest email >>= \req ->
  liftIO (withManager (httpLbs req)) >>= \res ->
  unless (responseStatus res == status200)
    (zlog ("Couldn't send email: " <> (pack . show $ res)) >> oops "Couldn't send email")
-}
bletch :: Int -> Int
bletch x = x
