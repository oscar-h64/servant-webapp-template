--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Util.Email (
    Attachments,
    emailTemplate,
    sendEmail,
    sendEmailFrom,
    sendEmailWithAttachments,
    sendEmailFromWithAttachments,
    emailThread
) where

--------------------------------------------------------------------------------

import Control.Concurrent            ( readChan, writeChan )
import Control.Monad                 ( forever )
import Control.Monad.Reader          ( MonadIO (liftIO) )

import Data.Maybe                    ( fromMaybe, maybeToList )
import Data.Text                     ( Text )
import Data.Text.Lazy                ( fromStrict )

import Network.Mail.Mime             ( Address (..), addAttachments, htmlPart,
                                       plainPart )
import Network.Mail.SMTP             ( sendMailWithLoginTLS, simpleMail )

import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Text.Hamlet                   ( Html, HtmlUrl )

import App.Types.Common              ( AppHandler, EmailChannel,
                                       Environment (..), askEnv )
import App.Types.Config              ( SMTPConfig (..) )
import App.Types.Routing             ( Page, PageData (..), getPagePath )

--------------------------------------------------------------------------------

type Attachments = [(Text, FilePath)]

--------------------------------------------------------------------------------

-- Emails can be rendered from templates. Use:
-- sendEmail to subject plainText $ emailTemplate $(hamletFile "emails/example")

emailTemplate :: HtmlUrl Page -> Maybe Html
emailTemplate html = pure $ html (\page _ -> getPagePath page)

--------------------------------------------------------------------------------

sendEmail :: Address -> Text -> Text -> Maybe Html -> AppHandler ()
sendEmail to subject textPart mHtmlPart =
    sendEmailWithAttachments to subject textPart mHtmlPart []

sendEmailFrom :: Address
              -> Address
              -> Text
              -> Text
              -> Maybe Html
              -> AppHandler ()
sendEmailFrom from to subject textPart mHtmlPart =
    sendEmailFromWithAttachments from to subject textPart mHtmlPart []

sendEmailWithAttachments :: Address
                         -> Text
                         -> Text
                         -> Maybe Html
                         -> Attachments
                         -> AppHandler ()
sendEmailWithAttachments to subject textPart mHtmlPart atts = do
    from <- askEnv envEmailDefaultFrom
    sendEmailFromWithAttachments from to subject textPart mHtmlPart atts

sendEmailFromWithAttachments :: Address
                             -> Address
                             -> Text
                             -> Text
                             -> Maybe Html
                             -> Attachments
                             -> AppHandler ()
sendEmailFromWithAttachments from to subject textPart mHtmlPart atts = do
    emailChan <- askEnv envEmailChannel

    let mHtmlLazy = htmlPart . renderHtml <$> mHtmlPart
    let parts = plainPart (fromStrict textPart) : maybeToList mHtmlLazy
    let email = simpleMail from [to] [] [] subject parts

    liftIO $ addAttachments atts email >>= writeChan emailChan

--------------------------------------------------------------------------------

emailThread :: Maybe SMTPConfig -> EmailChannel -> IO ()
emailThread mCfg chan = forever $ do
    -- Wait for the next email
    email <- readChan chan

    -- If the SMTP config is set then send the email, otherwise just send it
    -- to stdout
    case mCfg of
        Just MkSMTPConfig{..} ->
            sendMailWithLoginTLS smtpHostname smtpUsername smtpPassword email
        Nothing -> print "New Email:" >> print email

--------------------------------------------------------------------------------
