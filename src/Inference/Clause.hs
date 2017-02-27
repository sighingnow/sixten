{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Inference.Clause where

import Control.Monad.Except
import Data.Bifunctor
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen
import Text.Trifecta.Result(Err(Err), explain)

import Syntax
import Syntax.Concrete.Definition
import Syntax.Concrete.Pattern
import TCM

equalisePats
  :: (Pretty v)
  => [Plicitness]
  -> [(Plicitness, Pat e v)]
  -> TCM [(Plicitness, Pat e v)]
equalisePats [] pats = return pats
equalisePats _ [] = return []
equalisePats (Implicit:ps) ((Implicit, pat):pats)
  = (:) (Implicit, pat) <$> equalisePats ps pats
equalisePats (Explicit:ps) ((Explicit, pat):pats)
  = (:) (Explicit, pat) <$> equalisePats ps pats
equalisePats (Implicit:ps) pats@((Explicit, _):_)
  = (:) (Implicit, WildcardPat) <$> equalisePats ps pats
equalisePats (Explicit:_) ((Implicit, pat):_) = do
  loc <- currentLocation
  throwError
    $ show
    $ explain loc
    $ Err (Just "Explicit/implicit mismatch")
    [ "Found the implicit pattern:" Leijen.<+> Leijen.red (runPrettyM $ prettyAnnotation Implicit (prettyM $ first (const ()) pat)) <> "."
    , "Expected:" Leijen.<+> "an" Leijen.<+> Leijen.dullgreen "explicit" Leijen.<+> "pattern."
    ]
    mempty

equaliseClauses
  :: (AppSyntax expr, Applicative expr, Annotation expr ~ Plicitness)
  => NonEmpty (Clause expr v)
  -> NonEmpty (Clause expr v)
equaliseClauses clauses
  = NonEmpty.zipWith
    (uncurry etaClause)
    (go (Vector.toList . clausePatterns <$> clauses))
    (clauseScope <$> clauses)
  where
    go
      :: NonEmpty [(Plicitness, Pat (PatternScope expr v) ())]
      -> NonEmpty ([(Plicitness, Pat (PatternScope expr v) ())], [Plicitness])
    go clausePats
      | numEx == 0 && numIm == 0 = (\pats -> (pats, mempty)) <$> clausePats
      | numEx == len = NonEmpty.zipWith (first . (:)) heads $ go tails
      | numIm == len = NonEmpty.zipWith (first . (:)) heads $ go tails
      | numIm > 0 = go' $ addImplicit <$> clausePats
      | numEx > 0 = go' $ addExplicit <$> clausePats
      | otherwise = error "equaliseClauses go"
      where
        numEx = numExplicit clausePats
        numIm = numImplicit clausePats
        heads = head <$> clausePats
        tails = tail <$> clausePats
        len = length clausePats
    go'
      :: NonEmpty ([(Plicitness, Pat (PatternScope expr v) ())], [Plicitness])
      -> NonEmpty ([(Plicitness, Pat (PatternScope expr v) ())], [Plicitness])
    go' clausePats
      = NonEmpty.zipWith
        (\ps (pats, ps') -> (pats, ps ++ ps'))
        (snd <$> clausePats)
        (go $ fst <$> clausePats)

    numExplicit, numImplicit :: NonEmpty [(Plicitness, Pat (PatternScope expr v) ())] -> Int
    numExplicit = length . NonEmpty.filter (\xs -> case xs of
      (Explicit, _):_ -> True
      _ -> False)

    numImplicit = length . NonEmpty.filter (\xs -> case xs of
      (Implicit, _):_ -> True
      _ -> False)

    addImplicit, addExplicit
      :: [(Plicitness, Pat (PatternScope expr v) ())]
      -> ([(Plicitness, Pat (PatternScope expr v) ())], [Plicitness])
    addImplicit pats@((Implicit, _):_) = (pats, mempty)
    addImplicit pats = ((Implicit, WildcardPat) : pats, mempty)

    addExplicit pats@((Explicit, _):_) = (pats, mempty)
    addExplicit pats = ((Explicit, VarPat mempty ()) : pats, pure Explicit)

etaClause
  :: (AppSyntax expr, Applicative expr, Annotation expr ~ Plicitness)
  => [(Plicitness, Pat (PatternScope expr v) ())]
  -> [Plicitness]
  -> PatternScope expr v
  -> Clause expr v
etaClause pats extras (Scope scope)
  = Clause
    (Vector.fromList pats)
    $ Scope
    $ apps scope vs
  where
    numBindings = length $ concat $ Foldable.toList . snd <$> pats
    numExtras = length extras
    vs = zip extras $ pure . B . PatternVar <$> [numBindings - numExtras ..]