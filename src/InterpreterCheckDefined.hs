module InterpreterCheckDefined where

import Data.Maybe (catMaybes)

import Types
import PrettyShow
import InterpreterGetters


-- check if everything is defined
checkDefinedKL :: Klausel -> IS ()
checkDefinedKL (Klausel (Konsequenz kmt) (Bedingung bts)) = do
  let ts = catMaybes [kmt] ++ bts
  mapM_ checkDefinedTerm ts


checkDefinedTerm :: Term -> IS ()
checkDefinedTerm (Cut _) = return ()
checkDefinedTerm (TermR r ds) = do
  dts <- getRelationDecl r
  if length ds == length dts
     then mapM_ checkDefinedDasein ds
     else throw $ WrongNumberOfArgs $ psR r


-- returnes given Daseins inferred TypDasein and its subproblem
checkDefinedDasein :: Dasein -> IS ()
checkDefinedDasein (DaseinV _) = return ()
checkDefinedDasein (DaseinK k ds) = do
  (_, dts) <- getKonstruktorDecl k
  if length ds == length dts
     then mapM_ checkDefinedDasein ds
     else throw $ WrongNumberOfArgs $ psK k
