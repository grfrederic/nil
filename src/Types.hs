module Types where

import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad.Trans.Except as E
import qualified Data.Map as M
import qualified Text.Megaparsec as P
import Data.Void
import Data.Text (Text)


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
            | DaseinR Konstruktor [(Tag, Dasein)]
            deriving (Eq, Show)

data TypDasein = TypDaseinK TypKonstruktor [TypDasein]
               | TypDaseinV TypVariable
               deriving (Eq, Show)

data Term = Term Relation [Dasein] deriving (Eq, Show)
newtype Bedingung = Bedingung [Term] deriving (Eq, Show)
newtype Konsequenz = Konsequenz (Maybe Term) deriving (Eq, Show)


-- program line
data TypDefinition = KonstruktorTD Konstruktor [TypDasein] TypDasein
                   | RecordTD Konstruktor [(Tag, TypDasein)] TypDasein
                   | RelationTD Relation [TypDasein]
                   deriving (Eq, Show)

data Klausel = Klausel Konsequenz Bedingung deriving (Eq, Show)


-- program
newtype Program = Program [Either TypDefinition Klausel]
                  deriving (Eq, Show)


-- parser
type Parser = P.Parsec Void Text


-- interpreter
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


-- unification and SLD resolution
newtype Substitution = Substitution (M.Map Variable Dasein)
                       deriving (Eq, Show)


data Goal = Goal
  { era :: Integer
  , solution :: Substitution
  , terms :: [Term]
  } deriving Show

