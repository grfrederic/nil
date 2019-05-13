module InterpreterGetters where

import qualified Control.Monad.State.Lazy as S 
import qualified Data.Map as M

import Types
import PrettyShow


getKonstruktorDecl :: Konstruktor -> IS (TypDasein, [TypDasein])
getKonstruktorDecl k = do
  ke <- konstruktorEnv <$> S.get
  case M.lookup k ke of
    Just (kt, dts) -> return (kt, dts)
    _ -> throw . Undefined $ psK k


getRelationDecl :: Relation -> IS [TypDasein]
getRelationDecl r = do
  re <- relationEnv <$> S.get
  case M.lookup r re of
    Just dts -> return dts
    _ -> throw . Undefined $ psR r


getRecordDecl :: Konstruktor -> IS [Tag]
getRecordDecl k = do
  re <- recordEnv <$> S.get
  case M.lookup k re of
    Just dts -> return dts
    _ -> throw . Undefined $ psK k
