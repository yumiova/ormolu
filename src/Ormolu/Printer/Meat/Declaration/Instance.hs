{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  , p_tyFamInstDecl
  )
where

import Control.Arrow
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  ClsInstDecl {..} -> do
    txt "instance "
    case cid_poly_ty of
      HsIB {..} -> sitcc (located hsib_body p_hsType)
      XHsImplicitBndrs NoExt -> notImplemented "XHsImplicitBndrs"
    let binds = (getLoc &&& located' p_valDecl) <$> cid_binds
        sigs = (getLoc &&& located' p_sigDecl) <$> cid_sigs
        decls = sortBy (compare `on` fst) (toList binds <> sigs)
    if not (null decls)
      then do
        txt " where"
        breakpoint
        inci $ traverse_ snd decls
      else do
        newline
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"

p_tyFamInstDecl :: TyFamInstDecl GhcPs -> R ()
p_tyFamInstDecl = \case
  TyFamInstDecl {..} -> do
    txt "type instance "
    p_tyFamInstEqn tfid_eqn
