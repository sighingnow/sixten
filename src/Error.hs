{-# LANGUAGE OverloadedStrings #-}
module Error where

import Data.Monoid
import Data.Text(Text)
import Data.Text.Prettyprint.Doc(annotate, line)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Parsix.Highlight
import Text.Parsix.Position

import Pretty
import Util

data SourceLoc = SourceLocation
  { sourceLocFile :: FilePath
  , sourceLocSpan :: !Span
  , sourceLocSource :: Text
  , sourceLocHighlights :: Highlights
  } deriving (Eq, Ord, Show)

-- | Gives a summary (fileName:row:column) of the location
instance Pretty SourceLoc where
  pretty src
    = pretty (sourceLocFile src)
    <> ":" <> shower (visualRow loc + 1)
    <> ":" <> shower (visualColumn loc + 1)
    where
      loc = spanStart $ sourceLocSpan src

-- TODO handle spans and not just the start position
locationRendering :: SourceLoc -> Doc
locationRendering src = prettyPosition
  defaultStyle
  (spanStart $ sourceLocSpan src)
  (sourceLocSource src)
  (sourceLocHighlights src)

data ErrorKind
  = SyntaxError
  | TypeError
  | CommandLineError
  deriving Show

instance Pretty ErrorKind where
  pretty SyntaxError = "Syntax error"
  pretty TypeError = "Type error"
  pretty CommandLineError = "Command-line error"

data Error = Error
  { errorKind :: !ErrorKind
  , errorSummary :: !Doc
  , errorLocation :: !(Maybe SourceLoc)
  , errorFootnote :: !Doc
  } deriving Show

syntaxError, typeError :: Doc -> Maybe SourceLoc -> Doc -> Error
syntaxError = Error SyntaxError
typeError = Error TypeError

commandLineError :: Doc -> Doc -> Error
commandLineError h = Error CommandLineError h Nothing

instance Pretty Error where
  pretty (Error kind summary (Just loc) footnote)
    = pretty loc <> ":" PP.<+> annotate (color Red) (pretty kind) <> ":" PP.<+> summary
    <> line <> locationRendering loc
    <> line <> footnote
    <> line
  pretty (Error kind summary Nothing footnote)
    = annotate (color Red) (pretty kind) <> ":" <> summary
    <> line <> footnote
    <> line

printError :: Error -> IO ()
printError = putDoc . pretty
