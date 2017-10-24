{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Demo where

import Data.Text hiding (intercalate)
import Data.List (intercalate)

import System.Exit

import Network.Api.Postmark


fakeemail :: Email
fakeemail = defaultEmail {
    emailFrom = "demo-from@postmark.hs"
  , emailTo = ["demo-to@postmark.hs"]
  , emailSubject = "demo, yes it really is a demo"
  , emailTag = Just "demo"
  , emailHtml = Just "Hello world!"
  , emailTrackOpens = Just True
  , emailTrackLinks = Just HtmlOnly
  , emailReplyTo = "demo-reply-to@postmark.hs"
  }

testhttps :: IO (PostmarkResponse' Sent)
testhttps =
  request postmarkHttpsTest $ email fakeemail

testhttp :: IO (PostmarkResponse' Sent)
testhttp =
  request postmarkHttpTest $ email fakeemail

custom :: PostmarkSettings -> Text -> Text -> Text -> Text -> IO (PostmarkResponse' Sent)
custom settings from to subject html =
  request settings $ email defaultEmail {
      emailFrom = from
    , emailTo = [ to ]
    , emailSubject = subject
    , emailHtml = Just html
    , emailReplyTo = from
    }

customWithTemplate :: PostmarkSettings -> Text -> Text -> Maybe Int -> IO (PostmarkResponse' Sent)
customWithTemplate settings from to templateId' =
  request settings $ emailWithTemplate defaultEmailWithTemplate {
      templateId = maybe 0 id templateId'
    , emailFrom' = from
    , emailTo' = [ to ]
    , emailReplyTo' = from
    }

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

dispatch :: [String] -> IO (PostmarkResponse' Sent)
dispatch ("testhttp" : []) = testhttp
dispatch ("testhttps" : []) = testhttps
dispatch ("http" : key : from : to : subject : html : []) = custom (postmarkHttp . pack $ key) (pack from) (pack to) (pack subject) (pack html)
dispatch ("https" : key : from : to : subject : html : []) = custom (postmarkHttp . pack $ key) (pack from) (pack to) (pack subject) (pack html)
dispatch ("withtemplate-http" : key : from : to : templateId' : []) = customWithTemplate (postmarkHttp . pack $ key) (pack from) (pack to) (readMaybe templateId' :: Maybe Int)
dispatch _ = do
  putStrLn $ intercalate "\n" [
      "usage: postmark-demo testhttp   -- use test token to run against http api"
    , "    or postmark-demo testhttps  -- use test token to run against https api"
    , "    or postmark-demo http key from to subject html"
    , "                                -- use <key> token to run against http api"
    , "    or postmark-demo https key from to subject html"
    , "                                -- use <key> token to run against https api"
    , "    or postmark-demo withtemplate-http key from to subject html template-id template-model"
    , "                                -- use <key> token to run against http api, invoking specified template"
    , "    or postmark-demo withtemplate-https key from to subject html template-id template-model"
    , "                                -- use <key> token to run against https api, invoking specified template"
    ]
  exitFailure
