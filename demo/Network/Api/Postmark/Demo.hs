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
  , emailReplyTo = "demo-reply-to@postmark.hs"
  }

testhttps :: IO (PostmarkResponse' Sent)
testhttps =
  request postmarkHttpsTest $ email fakeemail

testhttp :: IO (PostmarkResponse' Sent)
testhttp =
  request postmarkHttpsTest $ email fakeemail

custom :: PostmarkSettings -> Text -> Text -> Text -> Text -> IO (PostmarkResponse' Sent)
custom settings from to subject html =
  request settings $ email defaultEmail {
      emailFrom = from
    , emailTo = [ to ]
    , emailSubject = subject
    , emailHtml = Just html
    , emailReplyTo = from
    }

dispatch :: [String] -> IO (PostmarkResponse' Sent)
dispatch ("testhttp" : []) = testhttp
dispatch ("testhttps" : []) = testhttps
dispatch ("http" : key : from : to : subject : html : []) = custom (postmarkHttp . pack $ key) (pack from) (pack to) (pack subject) (pack html)
dispatch ("https" : key : from : to : subject : html : []) = custom (postmarkHttp . pack $ key) (pack from) (pack to) (pack subject) (pack html)
dispatch _ = do
  putStrLn $ intercalate "\n" [
      "usage: postmark-demo testhttp   -- use test token to run against http api"
    , "    or postmark-demo testhttps  -- use test token to run against https api"
    , "    or postmark-demo http key from to subject html"
    , "                                -- use <key> token to run against http api"
    , "    or postmark-demo https key from to subject html"
    , "                                -- use <key> token to run against https api"
    ]
  exitFailure
