{-# LANGUAGE BangPatterns, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, Rank2Types, TemplateHaskell            #-}
module Lens where
import ClassyPrelude.Yesod
import Control.Lens
import Language.Haskell.TH
import Yesod.Default.Util  (WidgetFileSettings, widgetFileNoReload,
                            widgetFileReload)

-- | Rules for making lenses and traversals that precompose another 'Lens'. that won't interfere with Yesod Scaffold
persistClassyRules :: LensRules
persistClassyRules = classyRules_
  & generateSignatures .~ False
  & generateUpdateableOptics .~ True

persistMakeClassy :: Name -> Q [Dec]
persistMakeClassy = makeLensesWith persistClassyRules
