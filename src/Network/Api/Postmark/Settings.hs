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

type PostmarkApiToken = Text

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
