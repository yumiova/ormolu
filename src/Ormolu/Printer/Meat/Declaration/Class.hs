{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of type class declarations.

module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl
  )
where

import Control.Monad
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import GHC
import RdrName (RdrName (..))
import SrcLoc (Located)

p_classDecl :: Located RdrName -> LHsQTyVars GhcPs -> R ()
p_classDecl name tvars = do
  let HsQTvs {..} = tvars
  txt "class "
  p_rdrName name
  unless (null hsq_explicit) space
  spaceSep (located' p_hsTyVarBndr) hsq_explicit
