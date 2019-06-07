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
import Data.Function
import Data.List (sortBy)
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
  -> LHsBinds GhcPs
  -> R ()
p_classDecl ctx name tvars csigs _ = do
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
  -- GHC's AST does not necessarily store each kind of element in source
  -- location order. This happens because different declarations are stored in
  -- different lists. Consequently, to get all the declarations in proper order,
  -- they need to be manually sorted.
  let sigs = (getLoc &&& located' p_sigDecl) <$> csigs
      decls = snd <$> sortBy (compare `on` fst) sigs
  if not (null decls)
    then do
      txt " where"
      newline
      inci (sequence_ decls)
    else newline
