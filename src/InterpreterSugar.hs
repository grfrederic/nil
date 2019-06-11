module InterpreterSugar where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get)
import Data.Char (toUpper)

import Types
import InterpreterGetters


desugarProgrammzeile :: ProgrammzeileS -> IS [Programmzeile]
desugarProgrammzeile (Left td) = desugarTypDefinition td
desugarProgrammzeile (Right k) = return . Right <$> desugarKlausel k 


desugarTypDefinition :: TypDefinitionS -> IS [Programmzeile]
desugarTypDefinition (RelationTDS r ts) = do
  dr <- desugarRelation r
  dts <- mapM desugarTypDasein ts
  return [Left $ RelationTD dr dts]

desugarTypDefinition (KonstruktorTDS k ts t) = do
  dk <- desugarKonstruktor k
  dts <- mapM desugarTypDasein ts
  dt <- desugarTypDasein t
  let line0 = Left $ KonstruktorTD dk dts dt
  let lfrom = createFrom dk dt dts
  return $ line0:lfrom

desugarTypDefinition (RecordTDS k tts t) = do
  dk <- desugarKonstruktor k
  let tags = map fst tts
  let typs = map snd tts
  dtags <- mapM desugarTag tags
  dtyps <- mapM desugarTypDasein typs
  let dtts = zip dtags dtyps
  dt <- desugarTypDasein t
  let line0 = Left $ RecordTD dk dtts dt
  let lgets = createGets dk dt dtts
  return $ line0:lgets


createFrom :: Konstruktor -> TypDasein -> [TypDasein] -> [Programmzeile]
createFrom k t ts = [Left rel, Right kls]
  where
    rel = RelationTD r (t:ts)
    kls = Klausel (Konsequenz $ Just tm) (Bedingung [])

    r = createRelation k
    tm = TermR r (DaseinK k vs:vs)

    createRelation :: Konstruktor -> Relation
    createRelation (Konstruktor n) =
      Relation $ "from" ++ (toUpper (head n):tail n)

    vs :: [Dasein]
    vs = take (length ts) $
         map (DaseinV . Variable . ('~':) . show) ([0..]::[Integer])


createGets :: Konstruktor -> TypDasein -> [(Tag, TypDasein)] -> [Programmzeile]
createGets k t ttds = map Left rtds ++ map Right ks
    where
      tags = map fst ttds
      tds = map snd ttds
      rels = map createRelation tags 
      rtds = zipWith (curry createRelTD) rels tds

      createRelation :: Tag -> Relation
      createRelation (Tag n) = Relation $ "get" ++ (toUpper (head n):tail n)

      createRelTD :: (Relation, TypDasein) -> TypDefinition
      createRelTD (r, t0) = RelationTD r [t, t0]

      simpleKlausel :: Term -> Klausel
      simpleKlausel tm = Klausel (Konsequenz $ Just tm) (Bedingung [])

      unpacker :: (Relation, Dasein) -> Term
      unpacker (r, d) = TermR r [constr, d]

      ks = map (simpleKlausel . unpacker) $ zip rels vs 

      constr :: Dasein
      constr = DaseinK k vs

      vs :: [Dasein]
      vs = take (length ttds) $
           map (DaseinV . Variable . ('~':) . show) ([0..]::[Integer])


desugarTypDasein :: TypDaseinS -> IS TypDasein
desugarTypDasein (TypDaseinVS (TypVariableS n)) =
  return (TypDaseinV (TypVariable n))
desugarTypDasein (TypDaseinKS tk tds) = do
  dtk <- desugarTypKonstruktor tk
  dtds <- mapM desugarTypDasein tds
  return $ TypDaseinK dtk dtds


desugarTypKonstruktor :: TypKonstruktorS -> IS TypKonstruktor
desugarTypKonstruktor (TypKonstruktorS tk) =
  return $ TypKonstruktor tk


desugarKlausel :: KlauselS -> IS Klausel
desugarKlausel (KlauselS (KonsequenzS kmt) (BedingungS bts)) = do
  dkmt <- case kmt of
    Just t -> Just <$> desugarTerm t
    Nothing -> return Nothing
  dbts <- mapM desugarTerm bts
  return $ Klausel (Konsequenz dkmt) (Bedingung dbts)


desugarTerm :: TermS -> IS Term
desugarTerm CutS = return $ Cut Nothing
desugarTerm (TermRS r ds) = do
  dr <- desugarRelation r
  dds <- mapM desugarDasein ds
  return $ TermR dr dds



desugarRelation :: RelationS -> IS Relation
desugarRelation (RelationS n) = return $ Relation n


desugarDasein :: DaseinS -> IS Dasein
desugarDasein (DaseinVS (VariableS v)) = return $ DaseinV (Variable v)
desugarDasein (DaseinKS k ds) = do
  dk <- desugarKonstruktor k
  dds <- mapM desugarDasein ds
  return $ DaseinK dk dds
desugarDasein (DaseinRS k tds) = do
  dk <- desugarKonstruktor k
  ts <- getRecordDecl dk
  pds <- mapM pick ts
  dds <- mapM desugarDasein pds
  return $ DaseinK dk dds
    where
      pick :: Tag -> IS DaseinS
      pick t = do
        st <- resugarTag t
        return $ M.fromList tds M.! st


desugarKonstruktor :: KonstruktorS -> IS Konstruktor
desugarKonstruktor (KonstruktorS n) = return $ Konstruktor n


desugarTag :: TagS -> IS Tag
desugarTag (TagS n) = return $ Tag n


-- resugar, mainly for printing
resugarDasein :: Dasein -> IS DaseinS
resugarDasein (DaseinV (Variable v)) = return $ DaseinVS (VariableS v)
resugarDasein (DaseinK k ds) = do
  sk <- resugarKonstruktor k
  re <- recordEnv <$> get
  sds <- mapM resugarDasein ds
  case M.lookup k re of
    Just ts -> do
      sts <- mapM resugarTag ts
      return $ DaseinRS sk (zip sts sds)
    Nothing -> return $ DaseinKS sk sds


resugarKonstruktor :: Konstruktor -> IS KonstruktorS
resugarKonstruktor (Konstruktor n) = return $ KonstruktorS n


resugarTag :: Tag -> IS TagS
resugarTag (Tag n) = return $ TagS n


resugarVariable :: Variable -> IS VariableS
resugarVariable (Variable n) = return $ VariableS n


resugarSubstitution :: Substitution -> IS SubstitutionS
resugarSubstitution (Substitution sm) = do
  let sl = M.toList sm
  let ks = map fst sl
  let vs = map snd sl
  sks <- mapM resugarVariable ks
  svs <- mapM resugarDasein vs
  return $ SubstitutionS $ M.fromList $ zip sks svs
