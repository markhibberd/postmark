{-# LANGUAGE OverloadedStrings #-}
module Postmark.Defaults where

import qualified Data.Map as M
import Data.Text

import Postmark.Data

-- FIX name??
testKey :: Text
testKey = "POSTMARK_API_TEST"

defaultEmail :: Email
defaultEmail = Email {
    emailFrom = []
  , emailTo = []
  , emailCc = []
  , emailBcc = []
  , emailSubject = ""
  , emailTag = Nothing
  , emailHtml = Nothing
  , emailText = Nothing
  , emailReplyTo = ""
  , emailHeaders = M.empty
  , emailAttachments = []
  }

