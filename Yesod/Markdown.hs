{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
--
-- Rewrite/simplification of yesod-markdown written by ajdunlap.
--
-- Forked from <https://github.com/ajdunlap/yesod-markdown>.
--
-------------------------------------------------------------------------------
module Yesod.Markdown
  ( Markdown(..)
  -- * Wrappers
  , markdownToHtml
  , markdownToHtmlTrusted
  , markdownFromFile
  -- * Conversions
  , parseMarkdown
  , writePandoc
  , writePandocTrusted
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultReaderOptions
  -- * Form helper
  , markdownField
  )
  where

import Yesod.Form (ToField(..), areq, aopt)
import Yesod.Form.Functions (parseHelper)
import Yesod.Core (RenderMessage)
import Yesod.Form.Types
import Yesod.Widget (toWidget)
import Text.Hamlet (hamlet, Html)
import Database.Persist (PersistField)

import Text.Blaze (ToMarkup (toMarkup))
import Text.Blaze.Html (preEscapedToMarkup)
import Text.Pandoc
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Data.Monoid      (Monoid)
import Data.String      (IsString)
import System.Directory (doesFileExist)

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

newtype Markdown = Markdown { unMarkdown :: Text }
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = markdownToHtml

instance ToField Markdown master where
    toField = areq markdownField

instance ToField (Maybe Markdown) master where
    toField = aopt markdownField

markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
    , fieldEnctype = UrlEncoded
    }

markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultReaderOptions

-- | No HTML sanitization
markdownToHtmlTrusted :: Markdown -> Html
markdownToHtmlTrusted = writePandocTrusted yesodDefaultWriterOptions
                      . parseMarkdown yesodDefaultReaderOptions

-- | Returns the empty string if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists <- doesFileExist f
    content <-
        if exists
            then T.readFile f
            else return ""

    return $ Markdown content

writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedToMarkup . sanitizeBalance . T.pack . writeHtmlString wo

writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = preEscapedToMarkup . writeHtmlString wo

parseMarkdown :: ReaderOptions -> Markdown -> Pandoc
parseMarkdown ro = readMarkdown ro . T.unpack . unMarkdown

-- | Defaults plus Html5, minus WrapText
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = def
  { writerHtml5    = True
  , writerWrapText = False
  }

-- | Defaults plus Smart and ParseRaw
yesodDefaultReaderOptions :: ReaderOptions
yesodDefaultReaderOptions = def
    { readerSmart    = True
    , readerParseRaw = True
    }
