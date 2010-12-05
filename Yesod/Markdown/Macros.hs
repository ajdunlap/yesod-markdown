{-# LANGUAGE ViewPatterns, QuasiQuotes, FlexibleContexts, CPP #-}
-- | Macros allow users to access more advanced functionality from within Markdown syntax. There are two types
-- of macros, block and inline, which allow substitution of 'Block' and 'Inline' data, respectively. Macros
-- are called in a very similar fashion to shell programs: the argument string is split on whitespace. The
-- first word is the name of the macro, and the remaining words are the arguments. Option-parsing libraries
-- may be useful for interpreting the arguments.
--
-- Note: using 'blockMacros' and 'inlineMacros' at the same time with the same magic string and can lead to behavior
-- that depends on the order in which they are called, if any of the macro names are the same for block and inline
-- macros. However, if none of the macro names are the same, unrecognized macro names will be ignored by the pass
-- that doesn't recognize them, leaving them available to be recognized by the other pass.

module Yesod.Markdown.Macros
  where

import Text.Pandoc
import Safe
import Yesod
import Control.Applicative
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.UTF8 as U

-- | Convert block-level macros. Block-level macros are signalled by a first-level header containing a piece of
-- inline code starting with a client-specified magic string. For example, if the magic string is @??@, a macro
-- can be called by
--
-- > #`??MACRO_NAME MACRO_ARGS`
--
-- where @MACRO_NAME@ is the identifying name of the macro and @MACRO_ARGS@ is a space-separated list of arguments.
blockMacros
  :: Yesod master
  => String                                             -- ^ Magic string to introduce the macro
  -> Map String ([String] -> GHandler sub master Block) -- ^ Lookup table from macro names to macro functions
  -> Pandoc
  -> GHandler sub master Pandoc
blockMacros magic table p = processWithM blockMacros' p where
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
  => String                                                -- ^ Magic string to introduce the macro
  -> Map String ([String] -> GHandler sub master [Inline]) -- ^ Lookup table from macro names to macro functions
  -> Pandoc
  -> GHandler sub master Pandoc
inlineMacros magic table p = processWithM (fmap concat . mapM inlineMacros') p where
  inlineMacros' (Code (splitAt (length magic) -> (magic',words -> ((flip Map.lookup table -> Just f):xs))))
    | magic == magic' = f xs
  inlineMacros' b = return [b]

-- | Convert a 'Hamlet' value to a 'Block'.
hamletToBlock :: Hamlet (Route master) -> GHandler sub master Block
hamletToBlock x = RawHtml . U.toString . renderHtml . x <$> getUrlRenderParams

-- | Convert a 'Hamlet' value to an 'Inline'.
hamletToInline :: Hamlet (Route master) -> GHandler sub master Inline
hamletToInline x = HtmlInline . U.toString . renderHtml . x <$> getUrlRenderParams

-- | Read in 
localRoute :: Read (Route master) => [String] -> GHandler sub master Inline
localRoute = maybe (return (Str "")) f . readMay . unwords where
  f = hamletToInline . (\x ->
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
@x@|])
