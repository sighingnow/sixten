{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, Rank2Types, ViewPatterns #-}
module Syntax.ConstantSize where

import Control.Monad
import Data.Bifunctor
import Data.Monoid
import qualified Data.Set as S
import Data.String
import Prelude.Extras

import Syntax
import Util

type Size = Int

-- | Expressions with variables of type @v@.
data Expr v
  = Var v
  | Global Name
  | Con QConstr
  | Lit Literal
  | Pi  !NameHint !Annotation Size (Scope1 Expr v)
  | Lam !NameHint !Annotation Size (Scope1 Expr v)
  | App  (Expr v) !Annotation (Expr v)
  | Case (Expr v) (Branches QConstr Expr v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-------------------------------------------------------------------------------
-- * Views and smart constructors
pi_ :: Name -> Annotation -> Size -> Expr Name -> Expr Name
pi_ n p t e = Pi (Hint $ Just n) p t $ abstract1 n e

lam :: Name -> Annotation -> Size -> Expr Name -> Expr Name
lam n p t e = Lam (Hint $ Just n) p t $ abstract1 n e

etaLam :: Hint (Maybe Name) -> Annotation -> Size -> Scope1 Expr v -> Expr v
etaLam _ p _ (Scope (App e p' (Var (B ()))))
  | B () `S.notMember` toSet (second (const ()) <$> e) && p == p'
    = join $ unvar (error "etaLam impossible") id <$> e
etaLam n p sz s = Lam n p sz s

globals :: Expr v -> Expr (Var Name v)
globals expr = case expr of
  Var v       -> Var $ F v
  Global g    -> Var $ B g
  Con c       -> Con c
  Lit l       -> Lit l
  Pi  x p sz s -> Pi x p sz (exposeScope globals s)
  Lam x p sz s -> Lam x p sz (exposeScope globals s)
  App e1 p e2 -> App (globals e1) p (globals e2)
  Case e brs  -> Case (globals e) (exposeBranches globals brs)

-------------------------------------------------------------------------------
-- Instances
instance Syntax Expr where
  pi_ n p (Lit sz) = Pi n p $ fromIntegral sz
  pi_ _ _ _ = error "ConstantSize.pi_"

  piView (Pi n p sz s) = Just (n, p, Lit $ fromIntegral sz, s)
  piView _ = Nothing

  lamView (Lam n p sz s) = Just (n, p, Lit $ fromIntegral sz, s)
  lamView _ = Nothing

  app = App

  appView (App e1 p e2) = Just (e1, p, e2)
  appView _ = Nothing

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr

instance Applicative Expr where
  pure = return
  (<*>) = ap

instance Monad Expr where
  return = Var
  expr >>= f = case expr of
    Var v        -> f v
    Global g     -> Global g
    Con c        -> Con c
    Lit l        -> Lit l
    Pi  x p sz s -> Pi x p sz (s >>>= f)
    Lam x p sz s -> Lam x p sz (s >>>= f)
    App e1 p e2  -> App (e1 >>= f) p (e2 >>= f)
    Case e brs   -> Case (e >>= f) (brs >>>= f)

instance (Eq v, IsString v, Pretty v) => Pretty (Expr v) where
  prettyM expr = case expr of
    Var v     -> prettyM v
    Global g  -> prettyM g
    Con c     -> prettyM c
    Lit l     -> prettyM l
    Pi  _ a t (unusedScope -> Just e) -> parens `above` arrPrec $
      prettyAnnotation a (prettyM t)
      <+> prettyM "->" <+>
      associate arrPrec (prettyM e)
    (bindingsViewM usedPiView -> Just (tele, s)) -> withTeleHints tele $ \ns ->
      parens `above` absPrec $
      prettyM "forall" <+> prettyTeleVarTypes ns tele <> prettyM "." <+>
      prettyM (instantiateTele (pure . fromText <$> ns) s)
    Pi {} -> error "impossible prettyPrec pi"
    (bindingsViewM lamView -> Just (tele, s)) -> withTeleHints tele $ \ns ->
      parens `above` absPrec $
      prettyM "\\" <> prettyTeleVarTypes ns tele <> prettyM "." <+>
      prettyM (instantiateTele (pure . fromText <$> ns) s)
    Lam {} -> error "impossible prettyPrec lam"
    App e1 a e2 -> prettyApp (prettyM e1) (prettyAnnotation a $ prettyM e2)
    Case e brs -> parens `above` casePrec $
      prettyM "case" <+> inviolable (prettyM e) <+> prettyM "of" <$$> indent 2 (prettyM brs)