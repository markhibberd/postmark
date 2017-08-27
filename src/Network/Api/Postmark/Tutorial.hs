{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-| @postmark@ is a haskell toolkit for dealing with the postmarkapp.com api
    for sending and receiving email.
-}
module Network.Api.Postmark.Tutorial (
    -- * Settings
    -- $settings

    -- * Sending Mail
    -- $email

    -- * Handling Responses
    -- $response
    ) where

import Network.Api.Postmark

{- $settings



> postmarkHttp "<your api token>"   -- Build PostmarkSettings to talk to
>                                   -- the _http_ api with you own api token.
> postmarkHttps "<your api token>"  -- Build PostmarkSettings to talk to
>                                   -- the _https_ api with you own api token.
> postmarkHttpTest                  -- Build PostmarkSettings to talk to
>                                   -- the _http_ api with the public test token.
> postmarkHttpsTest                 -- Build PostmarkSettings to talk to
>                                   -- the _https_ api with the public test token.

-}

{- $email

Sending a single email.

> import Network.Api.Postmark
>
> request postmarkHttpTest $ email defaultEmail {
>       emailFrom = "demo-from@postmark.hs"
>     , emailTo = ["demo-to@postmark.hs"]
>     , emailSubject = "demo, yes it really is a demo"
>     , emailTag = Just "demo"
>     , emailHtml = Just "Hello world!"
>     , emailReplyTo = "demo-reply-to@postmark.hs"
>     }

Sending multiple emails.

> import Network.Api.Postmark
>
> demo = request postmarkHttpTest $ emails [
>     defaultEmail {
>       emailFrom = "demo-from@postmark.hs"
>     , emailTo = ["demo-to@postmark.hs"]
>     , emailSubject = "demo, yes it really is a demo"
>     , emailTag = Just "demo"
>     , emailHtml = Just "Hello world!"
>     , emailReplyTo = "demo-reply-to@postmark.hs"
>     }
>   , defaultEmail {
>       emailFrom = "demo-from@postmark.hs"
>     , emailTo = ["demo-to@postmark.hs"]
>     , emailSubject = "demo, yes it really is a demo"
>     , emailTag = Just "demo"
>     , emailHtml = Just "Hello world again!"
>     , emailReplyTo = "demo-reply-to@postmark.hs"
>     }
>   }
-}

{- $response

Checking if the response is a success, ignoring detail:

> import Network.Api.Postmark
>
> case result of
>   PostmarkSuccess _ -> True


Handling specific failure cases:

> import Network.Api.Postmark
>
> case result of
>   PostmarkSuccess a ->  undefined            -- Everything ok.
>   PostmarkUnauthorized -> undefined          -- Invalid api token.
>   PostmarkFailure (PostmarkError errortype message)
>                     -> undefined             -- A standard postmark failure.
>   PostmarkUnexpected reason code body message
>                     -> undefined             -- Unexpected failure (bug or
>                                              -- api change).

Handling known postmark failures:

> import Network.Api.Postmark
>
> case errortype of
>   PostmarkBadApiToken -> undefined
>   PostmarkInvalidEmail -> undefined
>   PostmarkSenderNotFound -> undefined
>   PostmarkSenderNotConfirmed -> undefined
>   PostmarkInvalidJson -> undefined
>   PostmarkIncompatibleJson -> undefined
>   PostmarkNotAllowed -> undefined
>   PostmarkInactive -> undefined
>   PostmarkBounceNotFound -> undefined
>   PostmarkBounceQueryException -> undefined
>   PostmarkJsonRequired -> undefined
>   PostmarkTooManyMessages -> undefined
>   PostmarkUnkownError Int -> undefined
-}
