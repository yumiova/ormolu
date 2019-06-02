{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  ClsInstDecl {..} -> do
    txt "instance "
    p_instNameDecl cid_poly_ty
    txt " where"
    breakpoint
  _ -> notImplemented "certain types of signature declarations"

p_instNameDecl :: LHsSigType GhcPs -> R ()
p_instNameDecl = \case
  HsIB {..} -> located hsib_body p_hsType
  XHsImplicitBndrs NoExt -> notImplemented "XHsImplicitBndrs"
