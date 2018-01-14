{-# LANGUAGE OverloadedStrings, GADTSyntax #-}
module Network.Api.Postmark.Error (
  PostmarkError (..),
  PostmarkErrorType (..)
) where

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
  | PostmarkTemplateQueryException
  | PostmarkTemplateNotFound
  | PostmarkTemplateLimitWouldBeExceeded
  | PostmarkTemplateNoDataReceived
  | PostmarkTemplateRequiredFieldMissing
  | PostmarkTemplateFieldTooLarge
  | PostmarkTemplateFieldInvalid
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
          1100 -> PostmarkTemplateQueryException
          1101 -> PostmarkTemplateNotFound
          1105 -> PostmarkTemplateLimitWouldBeExceeded
          1109 -> PostmarkTemplateNoDataReceived
          1120 -> PostmarkTemplateRequiredFieldMissing
          1121 -> PostmarkTemplateFieldTooLarge
          1122 -> PostmarkTemplateFieldInvalid
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
  show PostmarkTemplateQueryException =
    "The value of a GET parameter for the request is not valid."
  show PostmarkTemplateNotFound =
     "TemplateId not found. The TemplateId references a Template that does not exist, or is not associated with the Server specified for this request."
  show PostmarkTemplateLimitWouldBeExceeded =
     "Template limit would be exceeded. A Server may have up to 300 active templates, processing this request would exceed this limit."
  show PostmarkTemplateNoDataReceived =
      "No Template data received. You didn’t provide JSON body parameters in your request. Refer to the Template API reference for more details on required parameters."
  show PostmarkTemplateRequiredFieldMissing =
      "A required Template field is missing. A required field is missing from the body of the POST request."
  show PostmarkTemplateFieldTooLarge =
      "Template field is too large. One of the values of the request's body exceeds our size restrictions for that field."
  show PostmarkTemplateFieldInvalid =
      "A Templated field has been submitted that is invalid. One of the fields of the request body is invalid."
  show (PostmarkUnkownError code) =
    "An unexpected error code [" ++ show code ++ "] was retured from postmark."
