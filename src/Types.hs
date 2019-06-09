module Types where

import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad.Trans.Except as E
import qualified Data.Map as M
import qualified Text.Megaparsec as P
import Data.Void
import Data.Text (Text)


-- PART I : SUGARED TYPES

-- basic types
newtype KonstruktorS = KonstruktorS String deriving (Eq, Show, Ord)
newtype VariableS = VariableS String deriving (Eq, Show, Ord)
newtype TagS = TagS String deriving (Eq, Show, Ord)

newtype TypKonstruktorS = TypKonstruktorS String deriving (Eq, Show, Ord)
newtype TypVariableS = TypVariableS String deriving (Eq, Show, Ord)

newtype RelationS = RelationS String deriving (Eq, Show, Ord)


-- complex types
data DaseinS = DaseinKS KonstruktorS [DaseinS]
             | DaseinVS VariableS
             | DaseinRS KonstruktorS [(TagS, DaseinS)]
             deriving (Eq, Show)

data TypDaseinS = TypDaseinKS TypKonstruktorS [TypDaseinS]
                | TypDaseinVS TypVariableS
                deriving (Eq, Show)

data TermS = TermRS RelationS [DaseinS]
           | CutS
           deriving (Eq, Show)

newtype BedingungS = BedingungS [TermS] deriving (Eq, Show)
newtype KonsequenzS = KonsequenzS (Maybe TermS) deriving (Eq, Show)


-- program line
data TypDefinitionS = KonstruktorTDS KonstruktorS [TypDaseinS] TypDaseinS
                    | RecordTDS KonstruktorS [(TagS, TypDaseinS)] TypDaseinS
                    | RelationTDS RelationS [TypDaseinS]
                    deriving (Eq, Show)

data KlauselS = KlauselS KonsequenzS BedingungS deriving (Eq, Show)


-- program
type ProgrammzeileS = Either TypDefinitionS KlauselS
newtype ProgrammS = ProgrammS [ProgrammzeileS] deriving (Eq, Show)


-- unification and SLD resolution
newtype SubstitutionS = SubstitutionS (M.Map VariableS DaseinS)
                        deriving (Eq, Show)


-- PART II : CORE TYPES

-- basic types
newtype Konstruktor = Konstruktor String deriving (Eq, Show, Ord)
newtype Variable = Variable String deriving (Eq, Show, Ord)
newtype Tag = Tag String deriving (Eq, Show, Ord)

newtype TypKonstruktor = TypKonstruktor String deriving (Eq, Show, Ord)
newtype TypVariable = TypVariable String deriving (Eq, Show, Ord)

newtype Relation = Relation String deriving (Eq, Show, Ord)


-- complex types
data Dasein = DaseinK Konstruktor [Dasein]
            | DaseinV Variable
            deriving (Eq, Show)

data TypDasein = TypDaseinK TypKonstruktor [TypDasein]
               | TypDaseinV TypVariable
               deriving (Eq, Show)

data Term = TermR Relation [Dasein]
          | Cut
          deriving (Eq, Show)

newtype Bedingung = Bedingung [Term] deriving (Eq, Show)
newtype Konsequenz = Konsequenz (Maybe Term) deriving (Eq, Show)


-- program line
data TypDefinition = KonstruktorTD Konstruktor [TypDasein] TypDasein
                   | RecordTD Konstruktor [(Tag, TypDasein)] TypDasein
                   | RelationTD Relation [TypDasein]
                   deriving (Eq, Show)

data Klausel = Klausel Konsequenz Bedingung deriving (Eq, Show)


-- program
type Programmzeile = Either TypDefinition Klausel
newtype Programm = Programm [Programmzeile] deriving (Eq, Show)


-- unification and SLD resolution
newtype Substitution = Substitution (M.Map Variable Dasein)
                       deriving (Eq, Show)

substitute :: Substitution -> Dasein -> Dasein
substitute (Substitution m) (DaseinV v) | v `M.member` m = m M.! v
                                        | otherwise      = DaseinV v
substitute s (DaseinK k ds) = DaseinK k (map (substitute s) ds)


instance Monoid Substitution where
  mempty = Substitution M.empty
  mappend (Substitution m1) (Substitution m2) = Substitution $ M.union cm rm
    where
      cm = M.map (substitute $ Substitution m2) m1
      rm = M.difference m2 m1


-- PART III : INTERPRETER, UNIFICATION AND RESOLUTION


-- parser will return the sugarded, *S, types
type Parser = P.Parsec Void Text


-- but the interpreter only works with core, non *S, types
data EnvState = EnvState
  { konstruktorEnv :: M.Map Konstruktor (TypDasein, [TypDasein])
  , recordEnv :: M.Map Konstruktor [Tag]
  , relationEnv :: M.Map Relation [TypDasein]
  , klauselEnv :: M.Map Relation [Klausel]
  } deriving Show

startEnvState :: EnvState
startEnvState = EnvState
  { konstruktorEnv = M.empty
  , recordEnv = M.empty
  , relationEnv = M.empty
  , klauselEnv = M.empty
  }


data Error = Undefined String
           | Redefined String
           | TypeClash String TypDasein TypDasein
           | WrongNumberOfArgs String
           | WrongRecordTags String
           | TypeInferenceFailed
           | NotImplemented

type IS = E.ExceptT Error (S.State EnvState)

throw :: Error -> IS a
throw = E.throwE


-- type inference
newtype InferredTypes = InferredTypes (M.Map Variable TypDasein)

emptyInferredTypes :: InferredTypes
emptyInferredTypes = InferredTypes M.empty

data Goal = Goal { era :: Integer
                 , solution :: Substitution
                 , terms :: [Term]
                 }
          | Checkpoint
          deriving Show
