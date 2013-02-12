{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test where

import Yesod
import Yesod.Markdown
import Control.Applicative ((<$>))
import Network.Wai.Handler.Warp (run)

data App = App

mkYesod "App" [parseRoutes|
    / RootR GET POST
|]

type Form x = Html -> MForm App App (FormResult x, Widget)

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        hamletToRepHtml [hamlet|$newline never
            $doctype 5
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="my awesome site">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data TheForm = TheForm { formContent  :: Markdown }

theForm :: Form TheForm
theForm = renderDivs $ TheForm
    <$> areq markdownField "" Nothing

getRootR :: Handler RepHtml
getRootR = do
    ((res, form), enctype ) <- runFormPost theForm
    defaultLayout $ do
        setTitle "My title"

        let c = case res of
                    FormSuccess f -> formContent f
                    _             -> ""

        [whamlet|$newline never
            <h1>Markdown test

            <p>Enter some markdown:
            <form enctype="#{enctype}" method="post">
                ^{form}
                <input type="submit">

            <h3>Form data (ToMarkup instance):
            <p>#{c}

            <h3>Form data:
            <p>#{markdownToHtml $ c}

            <h3>Form data (trusted):
            <p>#{markdownToHtmlTrusted $ c}
            |]

postRootR :: Handler RepHtml
postRootR = getRootR

main :: IO ()
main = run 3000 =<< toWaiApp App
