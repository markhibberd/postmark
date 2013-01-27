{-# LANGUAGE OverloadedStrings, GADTSyntax #-}
module Network.Api.Postmark.Error where

import Control.Applicative

import Data.Aeson
import Data.Text

-- * Error types

data PostmarkError =
  PostmarkError {
      errorType :: PostmarkErrorType
    , errorMessage :: Text
    } deriving (Eq, Show)

data PostmarkErrorType =
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


instance FromJSON PostmarkError where
  parseJSON (Object o) = PostmarkError
    <$> (fmap (\code -> case code of
          0 -> PostmarkBadApiToken
          300 -> PostmarkInvalidEmail
          400 -> PostmarkSenderNotFound
          401 -> PostmarkSenderNotConfirmed
          402 -> PostmarkInvalidJson
          403 -> PostmarkIncompatibleJson
          405 -> PostmarkNotAllowed
          406 -> PostmarkInactive
          407 -> PostmarkBounceNotFound
          408 -> PostmarkBounceQueryException
          409 -> PostmarkJsonRequired
          410 -> PostmarkTooManyMessages
          _ -> PostmarkUnkownError code) (o .: "ErrorCode"))
    <*> (o .: "Message")
  parseJSON _ = fail "Invalid Postmark Error Response"

instance Show PostmarkErrorType where
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
