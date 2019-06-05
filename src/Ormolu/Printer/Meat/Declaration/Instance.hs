{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  , FamilyStyle (Associate, Free)
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
        tyfam_insts =
          (getLoc &&& located' (p_tyFamInstDecl Associate)) <$> cid_tyfam_insts
        decls = sortBy (compare `on` fst) (toList binds <> sigs <> tyfam_insts)
    if not (null decls)
      then do
        txt " where"
        breakpoint
        inci $ traverse_ snd decls
      else do
        newline
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"

data FamilyStyle
  = Associate
  | Free

p_tyFamInstDecl :: FamilyStyle -> TyFamInstDecl GhcPs -> R ()
p_tyFamInstDecl style = \case
  TyFamInstDecl {..} -> do
    txt $ case style of
      Associate -> "type "
      Free -> "type instance "
    p_tyFamInstEqn tfid_eqn
    newline
