{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  )
where

import Control.Monad
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  ClsInstDecl {..} -> do
    txt "instance "
    p_hsSigType cid_poly_ty
    txt " where"
    breakpoint
    forM_ cid_binds $ \c -> inci (located c p_valDecl)
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"

p_hsSigType :: LHsSigType GhcPs -> R ()
p_hsSigType = \case
  HsIB {..} -> located hsib_body p_hsType
  XHsImplicitBndrs NoExt -> notImplemented "XHsImplicitBndrs"
