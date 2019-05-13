module PrettyShow where

import qualified Data.Map as M
import Data.List (intercalate)

import Types


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
    psD' False (DaseinR k tds) =
      psK k ++ " {" ++ intercalate ", " (map psTagDas tds) ++ "}"
    psD' True d@(DaseinR _ _) =
      "(" ++ psD' False d ++ ")"
    psD' False (DaseinK k ds) =
      unwords (psK k : map (psD' True) ds)
    psD' True d@(DaseinK _ _) =
      "(" ++ psD' False d ++ ")"
    psTagDas (t, d) = psT t ++ ": " ++ psD d

psTD :: TypDasein -> String
psTD = psTD' False
  where
    psTD' _ (TypDaseinK k []) = psTK k
    psTD' _ (TypDaseinV v) = psTV v
    psTD' False (TypDaseinK k ds) =
      unwords (psTK k : map (psTD' True) ds)
    psTD' True (TypDaseinK k ds) =
      "(" ++ unwords (psTK k : map (psTD' True) ds) ++ ")"


psS :: Substitution -> String
psS (Substitution m) = join $ map showAssoc $ M.assocs m
  where
    join = intercalate "; "
    showAssoc (v, d) = unwords [psV v, "->", psD d]


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
