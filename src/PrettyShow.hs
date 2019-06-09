module PrettyShow where

import qualified Data.Map as M
import Data.List (intercalate)

import Types


-- PART I : SUGARED TYPES

-- basic types
psKS :: KonstruktorS -> String
psKS (KonstruktorS n) = n

psVS :: VariableS -> String
psVS (VariableS n) = n

psTS :: TagS -> String
psTS (TagS n) = n
 
psTKS :: TypKonstruktorS -> String
psTKS (TypKonstruktorS n) = n

psTVS :: TypVariableS -> String
psTVS (TypVariableS n) = n

psRS :: RelationS -> String
psRS (RelationS n) = n


-- complex types
psDS :: DaseinS -> String
psDS = psDS' False
  where
    psDS' _ (DaseinKS k []) = psKS k
    psDS' _ (DaseinVS v) = psVS v
    psDS' False (DaseinRS k tds) =
      psKS k ++ " {" ++ intercalate ", " (map psTagDasS tds) ++ "}"
    psDS' True d@(DaseinRS _ _) =
      "(" ++ psDS' False d ++ ")"
    psDS' False (DaseinKS k ds) =
      unwords (psKS k : map (psDS' True) ds)
    psDS' True d@(DaseinKS _ _) =
      "(" ++ psDS' False d ++ ")"
    psTagDasS (t, d) = psTS t ++ ": " ++ psDS d

psTDS :: TypDaseinS -> String
psTDS = psTDS' False
  where
    psTDS' _ (TypDaseinKS k []) = psTKS k
    psTDS' _ (TypDaseinVS v) = psTVS v
    psTDS' False (TypDaseinKS k ds) =
      unwords (psTKS k : map (psTDS' True) ds)
    psTDS' True (TypDaseinKS k ds) =
      "(" ++ unwords (psTKS k : map (psTDS' True) ds) ++ ")"


-- PART I : SUGARED TYPES

-- basic types
psK :: Konstruktor -> String
psK (Konstruktor n) = n

psV :: Variable -> String
psV (Variable n) = n

psT :: Tag -> String
psT (Tag n) = n
 
psTK :: TypKonstruktor -> String
psTK (TypKonstruktor n) = n

psTV :: TypVariable -> String
psTV (TypVariable n) = n

psR :: Relation -> String
psR (Relation n) = n


-- complex types
psD :: Dasein -> String
psD = psD' False
  where
    psD' _ (DaseinK k []) = psK k
    psD' _ (DaseinV v) = psV v
    psD' False (DaseinK k ds) =
      unwords (psK k : map (psD' True) ds)
    psD' True d@(DaseinK _ _) =
      "(" ++ psD' False d ++ ")"

psTD :: TypDasein -> String
psTD = psTD' False
  where
    psTD' _ (TypDaseinK k []) = psTK k
    psTD' _ (TypDaseinV v) = psTV v
    psTD' False (TypDaseinK k ds) =
      unwords (psTK k : map (psTD' True) ds)
    psTD' True (TypDaseinK k ds) =
      "(" ++ unwords (psTK k : map (psTD' True) ds) ++ ")"


-- PART III : SUBSTITUTION, ERRORS
psS :: SubstitutionS -> String
psS (SubstitutionS m) = join $ map showAssoc $ M.assocs m
  where
    join = intercalate "; "
    showAssoc (v, d) = unwords [psVS v, "->", psDS d]


-- errors
psE :: Error -> String
psE (Undefined n) = n ++ " WURDE NICHT DEFINIERT"
psE (Redefined n) = n ++ " WAR SCHON DEFINIERT"
psE (TypeClash n t1 t2) = n ++ " KANN NICHT GLEICHZEITIG "
                            ++ psTD t1 ++ " UND " ++ psTD t2 ++ " SEIN"
psE NotImplemented = "JA NATÜRLICH, WIESO NICHT"
psE (WrongNumberOfArgs n) = "FALSCHE ANZAHL VON ARGUMENTEN FÜR " ++ n
psE (WrongRecordTags n) = "FALSCHE ARGUMENTE FÜR " ++ n
psE TypeInferenceFailed = "TYPINFERENZ FEHLGESCHLAGEN"
