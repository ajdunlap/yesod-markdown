{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
-- | This module provides functions for using Markdown with Yesod. An example pipeline could be
--
-- > (writePandoc defaultWriterOptions <$>) . localLinks . parseMarkdown defaultParserState
module Yesod.Markdown
  ( Markdown (..)
  , parseMarkdown
  , localLinks
  , writePandoc
  , writePandocTrusted
    -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultParserState
  , yesodDefaultParserStateTrusted
    -- * Macros
    -- $macros
  , inlineMacros
  , blockMacros
  )
  where

import Yesod
import qualified Data.ByteString.Char8 as B
import Text.HTML.SanitizeXSS
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Data
import Control.Applicative
import Text.Pandoc
-- import Data.Maybe
import Text.Pandoc.Shared

newtype Markdown = Markdown String

-- | Write untrusted 'Pandoc' to 'Html'. Calls 'sanitizeBalance' from xss-sanitize.
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedString . sanitizeBalance . writeHtmlString wo

-- | Write trusted 'Pandoc' to 'Html'. Does not sanitize or balance tags.
writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = preEscapedString . writeHtmlString wo

-- | Read in 'Markdown' to an intermediate 'Pandoc' type.
parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

-- | Convert local (in-site) links. This function works on all link URLs that start with the two-character
-- prefix @~:@. It normalizes the links and replaces the @~:@ with the @approot@ value for the site.
localLinks :: Yesod master => Pandoc -> GHandler sub master Pandoc
localLinks p = (\y -> processWith (links y) p) <$> getYesod where
  links y (Link x ('~':':':l,n)) = Link x (joinPath y (approot y) (links' y (B.pack l)) [],n)
  links _ x = x
  links' y l = case splitPath y l of
    Left corrected -> links' y corrected
    Right xs       -> xs

-- $macros
-- Macros allow users to access more advanced functionality from within Markdown syntax. There are two types
-- of macros, block and inline, which allow substitution of 'Block' and 'Inline' data, respectively. Macros
-- are called in a very similar fashion to shell programs: the argument string is split on whitespace. The
-- first word is the name of the macro, and the remaining words are the arguments. Option-parsing libraries
-- may be useful for interpreting the arguments.
--
-- Note: using 'blockMacros' and 'inlineMacros' at the same time with the same magic string and can lead to behavior
-- that depends on the order in which they are called, if any of the macro names are the same for block and inline
-- macros. However, if none of the macro names are the same, unrecognized macro names will be ignored by the pass
-- that doesn't recognize them, leaving them available to be recognized by the other pass.

-- | Convert block-level macros. Block-level macros are signalled by a first-level header containing a piece of
-- inline code starting with a client-specified magic string. For example, if the magic string is @??@, a macro
-- can be called by
--
-- > #`??MACRO_NAME MACRO_ARGS`
--
-- where @MACRO_NAME@ is the identifying name of the macro and @MACRO_ARGS@ is a space-separated list of arguments.
blockMacros
  :: Yesod master
  => Map String ([String] -> GHandler sub master Block) -- ^ Lookup table from macro names to macro functions
  -> String                                             -- ^ Magic string to introduce the macro
  -> Pandoc
  -> GHandler sub master Pandoc
blockMacros table magic p = processWithM blockMacros' p where
  blockMacros' (Header 1 [Code (splitAt (length magic) -> (magic',words -> ((flip Map.lookup table -> Just f):xs)))])
    | magic == magic' = f xs
  blockMacros' b = return b

-- | Convert block-level macros. Inline-level macros are signalled by a piece of inline code starting with a
-- client-specified magic string. For example, if the magic string is @??@, a macro can be called by
--
-- > `??MACRO_NAME MACRO_ARGS`
--
-- where @MACRO_NAME@ is the identifying name of the macro and @MACRO_ARGS@ is a space-separated list of arguments.
inlineMacros
  :: Yesod master
  => Map String ([String] -> GHandler sub master Inline) -- ^ Lookup table from macro names to macro functions
  -> String                                              -- ^ Magic string to introduce the macro
  -> Pandoc
  -> GHandler sub master Pandoc
inlineMacros table magic p = processWithM inlineMacros' p where
  inlineMacros' (Code (splitAt (length magic) -> (magic',words -> ((flip Map.lookup table -> Just f):xs))))
    | magic == magic' = f xs
  inlineMacros' b = return b

-- | A set of default Pandoc writer options good for Yesod websites. Enables Javascript-based email obfuscation,
-- eliminates div tags around sections, and disables text wrapping.
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerEmailObfuscation = JavascriptObfuscation
  , writerSectionDivs = False
  , writerWrapText = False
  }

-- | A set of default Pandoc reader options good for Yesod websites /where the data input is trusted/. Enables raw
-- HTML.
yesodDefaultParserStateTrusted :: ParserState
yesodDefaultParserStateTrusted = yesodDefaultParserState { stateParseRaw = True }

-- | A set of default Pandoc reader options good for Yesod websites. Enables smart parsing.
yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState { stateSmart = True }
