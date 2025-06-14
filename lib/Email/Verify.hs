{-# LANGUAGE OverloadedStrings #-}

module Email.Verify where

import Data.Text.Lazy qualified as T
import Email.Email                 as Email
import Network.Mail.Mime
import Text.Blaze.Html5            ((!))
import Text.Blaze.Html5            qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Types.Email                 as TypeEmail
import Types.Name
import Types.User                  (User (..), UserEmail (..), UserName (..),
                                    UserVerificationToken (..))
import Types.VerificationToken
import GHC.Stack (HasCallStack)

verify ∷ HasCallStack => String → User → Email.Email
verify host User {
    email = UserEmail {
        getUserEmail = TypeEmail.Email {
            getEmail = email'
        }
    },
    name = UserName {
        getUserName = Name {
            getName = name'
        }
    },
    verificationToken = UserVerificationToken {
        getUserVerificationToken = Just VerificationToken {
            getVerificationToken = verificationToken'
        }
    }
} = Email.Email {
    to = Address (Just name') email',
    subject = "JobFinder: Verify your email address",
    text = "Hi " <> T.fromStrict name' <> ", you have been signed up to JobFinder. If this was you, please verify your email address at: " <> T.pack host <> "/verify?token=" <> T.show verificationToken' <> ", or if not, please disregard this email.",
    html = H.p $ do
        "Hi " <> H.text name' <> ", you have been signed up to JobFinder. If this was you, please verify your email address at: "
        (H.a ! A.href (H.lazyTextValue $ T.pack host <> "/verify?token=" <> T.show verificationToken'))
            . H.lazyText $ (T.pack host <> "/verify?token=" <> T.show verificationToken')
        ", or if not, please disregard this email."

}
verify _ User {
    verificationToken = UserVerificationToken {
        getUserVerificationToken = Nothing
    }
} = error "Verification token not present; should not be sending email."
