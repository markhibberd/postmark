{-# LANGUAGE OverloadedStrings #-}
module Network.Postmark.Data where

import Control.Applicative

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Data.Aeson
import Data.Map as M
import Data.Maybe
import Data.Monoid (mappend)
import Data.Text as T hiding (null)
import Data.List as L

-- FIX add default implementations for all data for convenient construction

type BatchEmail = [Email]

data Email = Email {
    emailFrom :: Text
  , emailTo :: [Text]
  , emailCc :: [Text]
  , emailBcc :: [Text]
  , emailSubject :: Text
  , emailTag :: Maybe Text
  , emailHtml :: Maybe Text
  , emailText :: Maybe Text
  , emailReplyTo :: Text
  , emailHeaders :: Map Text Text
  , emailAttachments :: [Attachment]
  }

data Attachment = Attachment {
    attachmentName :: Text
  , attachmentContent :: Text
  , attachmentContentType :: Text
  }

data PostmarkRequest a =
    HttpPostmarkRequest Text a
  | HttpsPostmarkRequest Text a

data PostmarkResponseSuccessData =
    PostmarkResponseSuccessData Text Text Text

data PostmarkResponseErrorData =
    PostmarkResponseErrorData Int Text


-- FIX consider smarter selectors for pulling out the data as a UTCTime or ZonedTime
data PostmarkResponse =
    PostmarkResponseSuccess {
        postmarkMessageId :: Text
      , postmarkSubmittedAt :: Text
      , postmarkTo :: Text
      }
  | PostmarkResponseUnauthorized
  | PostmarkResponseUnprocessible PostmarkError Text
  | PostmarkResponseServerError Text
  | PostmarkResponseInvalidResponseCode Int Text
  | PostmarkResponseJsonSyntaxError Int Text Text
  | PostmarkResponseJsonFormatError Int Text Text
  deriving (Eq, Show)

data PostmarkError =
    PostmarkBadApiToken
  | PostmarkInvalidEmail
  | PostmarkSenderNotFound
  | PostmarkSenderNotConfirmed
  | PostmarkInvalidJson
  | PostmarkIncompatibleJson
  | PostmarkNotAllowed
  | PostmarkInactive
  | PostmarkBounceNotFound
  | PostmarkBounceQueryException
  | PostmarkJsonRequired
  | PostmarkTooManyMessages
  | PostmarkUnkownError Int
  deriving Eq

instance ToJSON Email where
  toJSON v = object ([
      "From" .= (emailFrom v)
    , "To" .= T.intercalate "," (emailTo v)
    , "Subject" .= emailSubject v
    , "ReplyTo" .= emailReplyTo v
    ] ++ catMaybes [
      ojson "HtmlBody" (emailHtml v)
    , ojson "TextBody" (emailText v)
    , ojson "Tag" (emailTag v)
    , oljson "Cc" (emailCc v) (T.intercalate ",")
    , oljson "Bcc" (emailBcc v) (T.intercalate ",")
    , omjson "Headers" (emailHeaders v)
    , oljson "Attachments" (emailAttachments v) id
    ])

instance ToJSON Attachment where
  toJSON v = object [
      "Name" .= attachmentName v
    , "Content" .= attachmentContent v
    , "ContentType" .= attachmentContentType v
    ]

instance FromJSON PostmarkResponseSuccessData where
  parseJSON (Object o) = PostmarkResponseSuccessData
    <$> o .: "MessageID"
    <*> o .: "SubmittedAt"
    <*> o .: "To"
  parseJSON _ = fail "Invalid Postmark Success Response"

instance FromJSON PostmarkResponseErrorData where
  parseJSON (Object o) = PostmarkResponseErrorData
    <$> o .: "ErrorCode"
    <*> o .: "Message"
  parseJSON _ = fail "Invalid Postmark Error Response"


instance Show PostmarkError where
  show PostmarkBadApiToken =
    "Your request did not submit the correct API token in the X-Postmark-Server-Token header."
  show PostmarkInvalidEmail =
    "Validation failed for the email request JSON data that you provided."
  show PostmarkSenderNotFound =
    "You are trying to send email with a From address that does not have a sender signature."
  show PostmarkSenderNotConfirmed =
    "You are trying to send email with a From address that does not have a corresponding confirmed sender signature."
  show PostmarkInvalidJson =
    "The JSON input you provided is syntactically incorrect."
  show PostmarkIncompatibleJson =
    "The JSON input you provided is syntactically correct, but still not the one we expect."
  show PostmarkNotAllowed =
    "You ran out of credits."
  show PostmarkInactive =
    "You tried to send to a recipient that has been marked as inactive. Inactive recipients are ones that have generated a hard bounce or a spam complaint."
  show PostmarkBounceNotFound =
    "You requested a bounce by ID, but we could not find an entry in our database."
  show PostmarkBounceQueryException =
    "You provided bad arguments as a bounces filter."
  show PostmarkJsonRequired =
    "Your HTTP request does not have the Accept and Content-Type headers set to application/json."
  show PostmarkTooManyMessages =
    "Your batched request contains more than 500 messages."
  show (PostmarkUnkownError code) =
    "An unexpected error code [" ++ show code ++ "] was retured from postmark."

toPostmarkError :: Int -> PostmarkError
toPostmarkError 0 = PostmarkBadApiToken
toPostmarkError 300 = PostmarkInvalidEmail
toPostmarkError 400 = PostmarkSenderNotFound
toPostmarkError 401 = PostmarkSenderNotConfirmed
toPostmarkError 402 = PostmarkInvalidJson
toPostmarkError 403 = PostmarkIncompatibleJson
toPostmarkError 405 = PostmarkNotAllowed
toPostmarkError 406 = PostmarkInactive
toPostmarkError 407 = PostmarkBounceNotFound
toPostmarkError 408 = PostmarkBounceQueryException
toPostmarkError 409 = PostmarkJsonRequired
toPostmarkError 410 = PostmarkTooManyMessages
toPostmarkError code = PostmarkUnkownError code

toBaseUrl :: PostmarkRequest a -> Text
toBaseUrl (HttpPostmarkRequest _ _) = "http://api.postmarkapp.com/"
toBaseUrl (HttpsPostmarkRequest _ _) = "https://api.postmarkapp.com/"

toUrl :: PostmarkRequest a -> Text -> Text
toUrl req suffix = toBaseUrl req `mappend` suffix

postmarkToken :: PostmarkRequest a -> Text
postmarkToken (HttpPostmarkRequest t _) = t
postmarkToken (HttpsPostmarkRequest t _) = t

postmarkEmail :: PostmarkRequest a -> a
postmarkEmail (HttpPostmarkRequest _ e) = e
postmarkEmail (HttpsPostmarkRequest _ e) = e

successDataToResponse :: PostmarkResponseSuccessData -> PostmarkResponse
successDataToResponse (PostmarkResponseSuccessData ident at to) =
  PostmarkResponseSuccess ident at to

errorDataToResponse :: PostmarkResponseErrorData -> PostmarkResponse
errorDataToResponse (PostmarkResponseErrorData code message) =
  PostmarkResponseUnprocessible (toPostmarkError code) message

syntaxErr :: Int -> BL.ByteString -> Text -> PostmarkResponse
syntaxErr code body msg = PostmarkResponseJsonSyntaxError code msg (toText body)

decodeErr :: Int -> BL.ByteString -> Text -> PostmarkResponse
decodeErr code body msg = PostmarkResponseJsonFormatError code msg (toText body)

ojson :: ToJSON a => Text -> Maybe a -> Maybe (Text, Value)
ojson k = fmap (k .=)

oljson :: ToJSON b => Text -> [a] -> ([a] -> b) -> Maybe (Text, Value)
oljson k vs f = if L.null vs then Nothing else Just (k .= f vs)

omjson :: (ToJSON a) => Text -> Map Text a -> Maybe (Text, Value)
omjson k vs = if M.null vs then Nothing else Just (k .= vs)

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8
