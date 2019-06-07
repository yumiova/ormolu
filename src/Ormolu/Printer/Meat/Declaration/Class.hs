{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of type class declarations.

module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl
  )
where

import Control.Arrow
import Control.Monad
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import GHC
import RdrName (RdrName (..))
import SrcLoc (Located)

p_classDecl
  :: LHsContext GhcPs
  -> Located RdrName
  -> LHsQTyVars GhcPs
  -> [LSig GhcPs]
  -> R ()
p_classDecl ctx name tvars _ = do
  let HsQTvs {..} = tvars
  txt "class "
  sitcc $ do
    unless (null (unLoc ctx)) $ do
      located ctx p_hsContext
      breakpoint
      txt "=> "
    p_rdrName name
    unless (null hsq_explicit) space
    spaceSep (located' p_hsTyVarBndr) hsq_explicit
  newline
