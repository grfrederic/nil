module InterpreterCheckTypes where

import Control.Monad.State.Lazy as S 
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Char (toUpper)

import Types
import Unify
import PrettyShow
import InterpreterGetters


-- checkTypes if Klausel is well typed
checkTypesKL :: Klausel -> IS ()
checkTypesKL (Klausel (Konsequenz kmt) (Bedingung bts)) = do
  let ts = catMaybes [kmt] ++ bts
  subs <- zipWithM checkTypesTerm (map (:[]) [0..]) ts
  let unificationProblem = concat subs
  case unify unificationProblem of
    Just _ -> return ()
    _ -> throw TypeInferenceFailed 


checkTypesTerm :: [Int] -> Term -> IS [(Dasein, Dasein)]
checkTypesTerm mangleID (Term r ds) = do
  dts <- getRelationDecl r
  if length ds == length dts
     then do
       subs <- zipWithM checkTypesDasein (map (:mangleID) [0..]) ds
       let dtsInf = map fst subs
       let dtsPrb = subs >>= snd
       let dtsMan = map (mangleTypDasein mangleID) dts
       return $ zip dtsMan dtsInf ++ dtsPrb
     else throw $ WrongNumberOfArgs $ psR r


-- returnes given Daseins inferred TypDasein and its subproblem
checkTypesDasein :: [Int] -> Dasein -> IS (Dasein, [(Dasein, Dasein)])
checkTypesDasein mangleID (DaseinV v) = return (DaseinV $ varToVarType v, [])
checkTypesDasein mangleID (DaseinK k ds) = do
  (kt, dts) <- getKonstruktorDecl k
  if length ds == length dts
     then do
       subs <- zipWithM checkTypesDasein (map (:mangleID) [0..]) ds
       let dtsInf = map fst subs
       let dtsPrb = subs >>= snd
       let ktMan = mangleTypDasein mangleID kt
       let dtsMan = map (mangleTypDasein mangleID) dts
       return (ktMan, zip dtsMan dtsInf ++ dtsPrb)
     else throw $ WrongNumberOfArgs $ psK k


varToVarType :: Variable -> Variable
varToVarType (Variable v) = Variable nv
  where nv = '-' : map toUpper (tail v)


mangleTypDasein :: [Int] -> TypDasein -> Dasein
mangleTypDasein mangleID (TypDaseinK (TypKonstruktor tkn) tds) =
  DaseinK mk mds
    where
      mk = Konstruktor tkn
      mds = map (mangleTypDasein mangleID) tds

mangleTypDasein mangleID (TypDaseinV (TypVariable v)) =
  DaseinV $ Variable $ manglePrefix ++ v
    where
      manglePrefix = intercalate "_" $ ["-"] ++ map show mangleID ++ [":"]

