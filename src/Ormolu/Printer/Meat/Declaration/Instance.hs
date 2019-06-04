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
    case cid_poly_ty of
      HsIB {..} -> located hsib_body p_hsType
      XHsImplicitBndrs NoExt -> notImplemented "XHsImplicitBndrs"
    txt " where"
    breakpoint
    inci $ forM_ cid_binds $ \c -> located c p_valDecl
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"
