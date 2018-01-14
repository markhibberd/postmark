{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Settings (
  PostmarkSettings (..),
  PostmarkApiToken,
  postmarkTestToken,
  postmarkHttpTest,
  postmarkHttpsTest,
  postmarkHttp,
  postmarkHttps
) where

import Data.Text

-- | The Postmark “server token” which is sent via the
-- @X-Postmark-Server-Token@ HTTP header. You can find your server
-- token under the “Credentials” tab on the Postmark website.
--
-- If you do not yet have a Postmark account, or if you want to send
-- test emails that don't actually get delivered, you may use
-- 'postmarkTestToken'.
--
-- https://postmarkapp.com/developer/api/overview#authentication
type PostmarkApiToken = Text

-- | To construct 'PostmarkSettings', use 'postmarkHttps' or
-- 'postmarkHttp'.
--
-- Or to use the test API instead, use 'postmarkHttpsTest' or
-- 'postmarkHttpTest'.
data PostmarkSettings =
  PostmarkSettings {
      apiUrl :: Text
    , apiToken :: PostmarkApiToken
    } deriving (Eq, Show)

-- | An API token that you can use when you want to send test emails that
-- don't actually get delivered to the recipient.
--
-- https://postmarkapp.com/developer/api/overview#authentication
postmarkTestToken :: PostmarkApiToken
postmarkTestToken = "POSTMARK_API_TEST"

-- | Postmark settings using the HTTP protocol and the test API token
-- ('postmarkTestToken').
--
-- HTTPS is recommended instead ('postmarkHttpsTest').
postmarkHttpTest :: PostmarkSettings
postmarkHttpTest = postmarkHttp postmarkTestToken

-- | Postmark settings using the HTTPS protocol and the test API token
-- ('postmarkTestToken').
postmarkHttpsTest :: PostmarkSettings
postmarkHttpsTest = postmarkHttps postmarkTestToken

-- | Constructs Postmark settings using the HTTP protocol and your API token.
--
-- HTTPS is recommended instead ('postmarkHttps').
postmarkHttp :: PostmarkApiToken -> PostmarkSettings
postmarkHttp = PostmarkSettings "http://api.postmarkapp.com"

-- | Constructs Postmark settings using the HTTPS protocol and your API token.
postmarkHttps :: PostmarkApiToken -> PostmarkSettings
postmarkHttps = PostmarkSettings "https://api.postmarkapp.com"
