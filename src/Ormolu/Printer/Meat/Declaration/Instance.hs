{-# LANGUAGE LambdaCase #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Utils

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  _ -> notImplemented "certain types of signature declarations"
