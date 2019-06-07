{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of type class declarations.

module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl
  )
where

import Ormolu.Printer.Combinators
import GHC
import RdrName (RdrName (..))
import SrcLoc (Located)

p_classDecl :: Located RdrName -> LHsQTyVars GhcPs -> R ()
p_classDecl _ _ = pure ()
