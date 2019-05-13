module InterpreterSugar where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get)

import Types
import InterpreterGetters


-- change Klausel to desugered version
desugarKL :: Klausel -> IS Klausel
desugarKL (Klausel (Konsequenz kmt) (Bedingung bts)) = do
  dkmt <- case kmt of
    Just t -> Just <$> desugarTerm t
    Nothing -> return Nothing
  dbts <- mapM desugarTerm bts
  return $ Klausel (Konsequenz dkmt) (Bedingung dbts)


desugarTerm :: Term -> IS Term
desugarTerm (Term r ds) = do
  dds <- mapM desugarDasein ds
  return $ Term r dds


-- returnes given Daseins inferred TypDasein and its subproblem
desugarDasein :: Dasein -> IS Dasein
desugarDasein dv@(DaseinV _) = return dv
desugarDasein dk@(DaseinK _ _) = return dk
desugarDasein (DaseinR k tds) = do
  ts <- getRecordDecl k
  let dds = map pick ts
  return $ DaseinK k dds
    where
      pick :: Tag -> Dasein
      pick t = M.fromList tds M.! t


-- resugar Dasein, mainly for printing
resugarDasein :: Dasein -> IS Dasein
resugarDasein dr@(DaseinR _ _) = return dr
resugarDasein dv@(DaseinV _) = return dv
resugarDasein dk@(DaseinK k ds) = do
  re <- recordEnv <$> get
  case M.lookup k re of
    Just ts -> return $ DaseinR k (zip ts ds)
    Nothing -> return dk
