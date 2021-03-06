{-# LANGUAGE LambdaCase #-}
module Merger (mergeModules, extractFunNames) where
import Control.Applicative   ((<$>))
import Control.Applicative   ((<|>))
import Data.List             (nub)
import Data.List             (sort)
import Data.Maybe            (mapMaybe)
import Data.Maybe            (fromMaybe)
import Language.Haskell.Exts (Decl (..), Match (..), Module (..))
import Language.Haskell.Exts (ModuleName (..), Name, Pat (..), QName (..))

extractFunNames :: Decl -> [Name]
extractFunNames (FunBind ms) = [n | Match _ n _ _ _ _ <- ms]
extractFunNames (PatBind _ (PVar n) _ _) = [n]
extractFunNames (PatBind _ (PNPlusK n _) _ _) = [n]
extractFunNames (PatBind _ (PInfixApp _ (Qual _ n) _) _ _) = [n]
extractFunNames (PatBind _ (PInfixApp _ (UnQual n) _) _ _) = [n]
extractFunNames (PatBind l (PTuple _ pats) a b) =
  concatMap (\p -> extractFunNames (PatBind l p a b)) pats
extractFunNames (PatBind l (PList pats) a b) =
  concatMap (\p -> extractFunNames (PatBind l p a b)) pats
extractFunNames (PatBind l (PParen pat) a b) =
  extractFunNames (PatBind l pat a b)
extractFunNames (PatBind _ (PAsPat n _) _ _) = [n]
extractFunNames (PatBind l (PIrrPat pat) a b) =
  extractFunNames (PatBind l pat a b)
extractFunNames (PatBind l (PatTypeSig _ pat _) a b) =
  extractFunNames (PatBind l pat a b)
extractFunNames (PatBind l (PBangPat pat) a b) =
  extractFunNames (PatBind l pat a b)
extractFunNames _ = []

insertFunDecl :: Decl -> [Decl] -> [Decl]
insertFunDecl (TypeSig l names typ) ds =
  TypeSig l
  [n | n <- names
     , all (\case
               TypeSig _ ms _ -> n `notElem` ms
               _ -> True) ds
     ] typ : ds
insertFunDecl d ds = d:ds

shrink :: [Decl] -> [Decl] -> [Decl]
shrink add old =
  let exs = nub $ concatMap extractFunNames old
  in flip mapMaybe add $ \case
    (FunBind ms) ->
      let ms' = [m | m@(Match _ n _ _ _ _) <- ms, n `notElem` exs]
      in if null ms'
         then Nothing
         else Just $ FunBind ms'
    pb@(PatBind _ _ _ _)
      | any (`elem` exs) (extractFunNames pb) -> Nothing
    a -> Just a

-- | Right-biased module merge
mergeModules :: String -> Module -> Module -> Module
mergeModules n (Module _ _ ps1 wt1 mex1 imp1 ds1) (Module l _ ps2 wt2 mex2 imp2 ds2) =
  Module l (ModuleName n) (sort $ nub $ ps1 ++ ps2)
         (wt2 <|> wt1)
         ((fromMaybe [] mex1 ++) <$> mex2)
         (imp1 ++ imp2)
         (foldr (insertFunDecl) ds2 (shrink ds1 ds2))
