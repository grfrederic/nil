module InterpreterTypedefinition where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get, put)
import Data.Char (toUpper)

import Types
import PrettyShow
import InterpreterKlausel


-- updates EnvState with new Typdefinition
runTD :: TypDefinition -> IS ()
runTD (KonstruktorTD k ts t) = do
  s <- get
  let ke = konstruktorEnv s
  if k `M.member` ke
     then throw $ Redefined (psK k)
     else do
       put $ s {konstruktorEnv = M.insert k (t, ts) ke}
       addFrom k t ts

runTD (RecordTD k tts t) = do
  s <- get
  let ke = konstruktorEnv s
  let re = recordEnv s
  if k `M.member` ke
     then throw $ Redefined (psK k)
     else do
       let tags = map fst tts
       let typs = map snd tts
       put $ s { konstruktorEnv = M.insert k (t, typs) ke
               , recordEnv = M.insert k tags re
               }
       addGets k t tts

runTD (RelationTD r ts) = do
  s <- get
  let re = relationEnv s
  if r `M.member` re
     then throw $ Redefined (psR r)
     else put $ s {relationEnv = M.insert r ts re}


-- ex. cons ~A (List ~A) -> List ~A
-- creates:
-- fromCons ? (List ~A) ~A (List ~A)
-- fromCons (cons ~a ~l) ~a ~l
addFrom :: Konstruktor -> TypDasein -> [TypDasein] -> IS ()
addFrom k t ts = do
  runTD (RelationTD g (t:ts))
  addKL kl
  return ()
    where
      g = createRel k
      kl = Klausel (Konsequenz $ Just tm) (Bedingung [])

      createRel :: Konstruktor -> Relation
      createRel (Konstruktor (c:s)) = Relation $ "from" ++ (toUpper c:s)

      tm :: Term
      tm = Term g (DaseinK k vs:vs)

      vs :: [Dasein]
      vs = take (length ts) $
           map (DaseinV . Variable . ('~':) . show) ([0..]::[Integer])


-- ex. book : {id: Int, author: String} -> Book
-- creates:
-- getId ? Book Int
-- getId (book {id: ~i, author: ~a}) ~i
-- getAuthor ? Book String
-- getAuthor (book {id: ~i, author: ~a}) ~a
addGets :: Konstruktor -> TypDasein -> [(Tag, TypDasein)] -> IS ()
addGets k t ttds = do
  mapM_ runTD rtds
  mapM_ addKL ks
    where
      tags = map fst ttds
      tds = map snd ttds
      rels = map createRel tags 
      rtds = zipWith (curry createRelTD) rels tds

      createRel :: Tag -> Relation
      createRel (Tag (c:s)) = Relation $ "get" ++ (toUpper c:s)

      createRelTD :: (Relation, TypDasein) -> TypDefinition
      createRelTD (r, t0) = RelationTD r [t, t0]

      simpleKlausel :: Term -> Klausel
      simpleKlausel tm = Klausel (Konsequenz $ Just tm) (Bedingung [])

      unpacker :: (Relation, Dasein) -> Term
      unpacker (r, d) = Term r [constr, d]

      ks = map (simpleKlausel . unpacker) $ zip rels vs 

      constr :: Dasein
      constr = DaseinK k vs

      vs :: [Dasein]
      vs = take (length ttds) $
           map (DaseinV . Variable . ('~':) . show) ([0..]::[Integer])
