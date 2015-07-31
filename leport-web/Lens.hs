{-# LANGUAGE BangPatterns, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, Rank2Types, TemplateHaskell            #-}
module Lens where
import ClassyPrelude.Yesod
import Control.Lens
import Language.Haskell.Exts (Module (..), ModuleName, SrcLoc)
import Language.Haskell.Exts (ExportSpec)
import Language.Haskell.TH
import Yesod.Default.Util    (WidgetFileSettings, widgetFileNoReload,
                              widgetFileReload)

class HasSrcLoc a where
  _SrcLoc :: Lens' a SrcLoc

instance HasSrcLoc Module where
  _SrcLoc =
    lens
    (\(Module l _ _ps _wt _mex _imp _ds) -> l)
    (\(Module _ n ps wt mex imp ds) l ->  (Module l n ps wt mex imp ds))

_ExportSpecs :: Lens' Module (Maybe [ExportSpec])
_ExportSpecs = lens
  (\(Module _ _l _ps _wt mex _imp _ds) -> mex)
  (\(Module l n ps wt _ imp ds) mex ->
    Module l n ps wt mex imp ds)


_ModuleName :: Lens' Module ModuleName
_ModuleName =
    lens
    (\(Module _ l _ps _wt _mex _imp _ds) -> l)
    (\(Module n _ ps wt mex imp ds) l ->  (Module n l ps wt mex imp ds))


-- | Rules for making lenses and traversals that precompose another 'Lens'. that won't interfere with Yesod Scaffold
persistClassyRules :: LensRules
persistClassyRules = classyRules_
  & generateSignatures .~ False
  & generateUpdateableOptics .~ True

persistMakeClassy :: Name -> Q [Dec]
persistMakeClassy = makeLensesWith persistClassyRules


