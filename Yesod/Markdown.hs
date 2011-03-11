{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, QuasiQuotes, TypeFamilies, FlexibleInstances,
              MultiParamTypeClasses #-}
-- | This module provides functions for using Markdown with Yesod. An example pipeline could be
--
-- > (writePandoc defaultWriterOptions <$>) . localLinks . parseMarkdown defaultParserState
module Yesod.Markdown
  ( Markdown (..)
  , parseMarkdown
  , localLinks
  , writePandoc
    -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultParserState
  , yesodDefaultParserStateTrusted
   -- * Form helpers
  , markdownField
  , maybeMarkdownField
  )
  where

import Yesod
import Data.Monoid
import Data.String
import Yesod.Form.Core
import Text.Pandoc
import Text.Pandoc.Shared
import Control.Applicative
import Data.Map ( Map )
import qualified Data.Map as Map

newtype Markdown = Markdown String
  deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToFormField Markdown y where
  toFormField = markdownField

instance ToFormField (Maybe Markdown) y where
  toFormField = maybeMarkdownField

lines' :: String -> [String]
lines' = map go . lines where
  go [] = []
  go ('\r':[]) = []
  go (x:xs) = x : go xs

markdownField :: (IsForm f, FormType f ~ Markdown)
              => FormFieldSettings -> Maybe Markdown -> f
markdownField = requiredFieldHelper markdownFieldProfile

maybeMarkdownField :: FormFieldSettings -> FormletField sub y (Maybe Markdown)
maybeMarkdownField = optionalFieldHelper markdownFieldProfile

markdownFieldProfile :: FieldProfile sub y Markdown
markdownFieldProfile = FieldProfile
    { fpParse = Right . Markdown . unlines . lines'
    , fpRender = \(Markdown m) -> m
    , fpWidget = \theId name val _isReq -> addHamlet
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<textarea id="#{theId}" name="#{name}" .markdown>#{val}
|]
    }

-- | Write 'Pandoc' to 'Html'.
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedString . writeHtmlString wo

-- | Read in 'Markdown' to an intermediate 'Pandoc' type.
parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

-- | Convert local (in-site) links. This function works on all link URLs that start with the two-character
-- prefix @~:@. It normalizes the links and replaces the @~:@ with the @approot@ value for the site.
localLinks :: Yesod master => Pandoc -> GHandler sub master Pandoc
localLinks p = (\y -> processWith (links y) p) <$> getYesod where
  links y (Link x ('~':':':l,n)) = Link x (joinPath y (approot y) (links' y [l]) [],n)
  links _ x = x
  links' y l = case cleanPath y l of
    Left corrected -> links' y corrected
    Right xs       -> xs

-- | A set of default Pandoc writer options good for Yesod websites. Enables Javascript-based email obfuscation,
-- eliminates div tags around sections, and disables text wrapping.
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerEmailObfuscation = JavascriptObfuscation
  , writerSectionDivs = False
  , writerWrapText = False
  }

--
-- TODO: Pandoc dropped stateSanitizeHTML? or maybe you just can't shut 
-- it off...
--
-- | A set of default Pandoc reader options good for Yesod websites /where the data input is trusted/. Disables
-- HTML sanitization and enables smart parsing and raw HTML parsing.
yesodDefaultParserStateTrusted :: ParserState
yesodDefaultParserStateTrusted = yesodDefaultParserState

-- | A set of default Pandoc reader options good for Yesod websites. Enables smart parsing, raw HTML parsing, and
-- HTML sanitization.
yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState { stateSmart = True, stateParseRaw = True }
