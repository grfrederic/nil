module Parse where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Text.Megaparsec ((<|>))

import Types


-- simpler space, _does not_ consume newline
space :: Parser ()
space = do
  _ <- P.many (P.single ' ')
  return ()


-- simpler space, _does_ consume newline
spaceOrNL :: Parser ()
spaceOrNL = do
  _ <- P.many (P.single ' ' <|> P.single '\n')
  return ()


-- comment
comment :: Parser ()
comment =  L.skipLineComment (T.pack "--") 
       <|> L.skipLineComment (T.pack "<nic tu nie ma, nie ma czego parsować>")


-- basic types
konstruktorParser :: Parser Konstruktor
konstruktorParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . Konstruktor $ s0:rs


variableParser :: Parser Variable
variableParser = do
  s0 <- P.single '~'
  s1 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . Variable $ s0:s1:rs


tagParser :: Parser Tag
tagParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . Tag $ s0:rs


typKonstruktorParser :: Parser TypKonstruktor
typKonstruktorParser = do
  s0 <- C.upperChar
  rs <- P.many C.alphaNumChar
  return . TypKonstruktor $ s0:rs


typVariableParser :: Parser TypVariable
typVariableParser = do
  s0 <- P.single '~'
  s1 <- C.upperChar
  rs <- P.many C.alphaNumChar
  return . TypVariable $ s0:s1:rs


relationParser :: Parser Relation
relationParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . Relation $ s0:rs


-- complex types
daseinParserA :: Parser Dasein
daseinParserA =  fmap DaseinV variableParser
             <|> bracketed
             <|> P.try record
             <|> komplex
  where
    bracketed = do
      d <- P.single '(' >> space >> daseinParserA
      _ <- space >> P.single ')'
      return d

    komplex = do
      k <- konstruktorParser
      _ <- space
      ds <- daseinParserB `P.sepEndBy` space
      return $ DaseinK k ds

    record = do
      k <- konstruktorParser
      _ <- space >> P.single '{' >> space
      tds <- tagDasein `P.sepBy` (space >> P.single ',' >> space)
      _ <- space >> P.single '}'
      return $ DaseinR k tds 

    tagDasein = do
      t <- tagParser
      _ <- space >> P.single ':' >> space
      d <- daseinParserA
      return (t, d)


daseinParserB :: Parser Dasein
daseinParserB =  fmap DaseinV variableParser
             <|> bracketed
             <|> P.try record
             <|> simple
  where
    bracketed = do
      d <- P.single '(' >> space >> daseinParserA
      _ <- space >> P.single ')'
      return d

    simple = do
      k <- konstruktorParser
      return $ DaseinK k []

    record = do
      k <- konstruktorParser
      _ <- space >> P.single '{' >> space
      tds <- tagDasein `P.sepBy` (space >> P.single ',' >> space)
      _ <- space >> P.single '}'
      return $ DaseinR k tds 

    tagDasein = do
      t <- tagParser
      _ <- space >> P.single ':' >> space
      d <- daseinParserA
      return (t, d)


typDaseinParserA :: Parser TypDasein
typDaseinParserA =  fmap TypDaseinV typVariableParser
                <|> bracketed
                <|> komplex
  where
    bracketed = do
      t <- P.single '(' >> space >> typDaseinParserA
      _ <- space >> P.single ')'
      return t

    komplex = do
      t <- typKonstruktorParser
      _ <- space
      ts <- typDaseinParserB `P.sepEndBy` space
      return $ TypDaseinK t ts


typDaseinParserB :: Parser TypDasein
typDaseinParserB =  fmap TypDaseinV typVariableParser
                <|> bracketed
                <|> simple
  where
    bracketed = do
      t <- P.single '(' >> space >> typDaseinParserA
      _ <- space >> P.single ')'
      return t

    simple = do
      t <- typKonstruktorParser
      return $ TypDaseinK t []


termParser :: Parser Term
termParser = do
  r <- relationParser
  _ <- space
  ds <- daseinParserB `P.sepEndBy1` space
  return $ Term r ds


bedingungParser :: Parser Bedingung
bedingungParser = fmap Bedingung $ termParser `P.sepBy` comma
  where
    comma = space >> P.single ',' >> spaceOrNL


konsequenzParserJ :: Parser Konsequenz
konsequenzParserJ =  fmap (Konsequenz . Just) termParser

konsequenzParserN :: Parser Konsequenz
konsequenzParserN =  return $ Konsequenz Nothing

konsequenzParser :: Parser Konsequenz
konsequenzParser =  P.try konsequenzParserJ <|> konsequenzParserN


-- program line
konstruktorTDParser :: Parser TypDefinition
konstruktorTDParser =  P.try normal -- a : A B C -> D
                   <|> short        -- a : A
  where
    normal = do
      k <- konstruktorParser
      _ <- space >> P.single ':' >> space
      ts <- typDaseinParserB `P.sepEndBy` space
      _ <- space >> C.string (T.pack "->") >> space
      rt <- typDaseinParserA
      return $ KonstruktorTD k ts rt

    short = do
      k <- konstruktorParser
      _ <- space >> P.single ':' >> space
      rt <- typDaseinParserA
      return $ KonstruktorTD k [] rt


recordTDParser :: Parser TypDefinition
recordTDParser = do -- book : {id: Int, author: String} -> Book
  k <- konstruktorParser
  _ <- space >> P.single ':' >> space >> P.single '{'
  ttds <- tagTypDasein `P.sepBy` (space >> P.single ',' >> space)
  _ <- space >> P.single '}' >> space >> C.string (T.pack "->") >> space
  rt <- typDaseinParserA
  return $ RecordTD k ttds rt
    where
      tagTypDasein = do
        t <- tagParser
        _ <- space >> P.single ':' >> space
        td <- typDaseinParserA
        return (t, td)


relationTDParser :: Parser TypDefinition
relationTDParser = do
  r <- relationParser
  _ <- space >> P.single '?' >> space
  ts <- typDaseinParserB `P.sepEndBy1` space
  return $ RelationTD r ts


typDefinitionParser :: Parser TypDefinition
typDefinitionParser =  P.try konstruktorTDParser
                   <|> P.try recordTDParser
                   <|> relationTDParser


klauselParser :: Parser Klausel
klauselParser =  P.try full    -- k1 <- b1, ..., bn
             <|> P.try shortQ  -- b1, ..., bn?
             <|> P.try shortS  -- k1
  where
    full = do
      k <- konsequenzParser
      _ <- space >> C.string (T.pack "<-") >> space
      b <- bedingungParser
      return $ Klausel k b

    shortS = do
      k <- konsequenzParserJ
      return $ Klausel k (Bedingung [])

    shortQ = do
      b <- bedingungParser
      _ <- space >> P.single '?' >> space
      return $ Klausel (Konsequenz Nothing) b


-- program
programLineParser :: Parser (Maybe (Either TypDefinition Klausel))
programLineParser =  do
  _ <- space
  l <- fmap (Just . Left) (P.try typDefinitionParser)
   <|> fmap (Just . Right) (P.try klauselParser)
   <|> return Nothing
  _ <- space >> P.optional (P.try comment)
  _ <- P.single '\n'
  return l


programParser :: Parser Program
programParser = fmap (Program . catMaybes) $ P.many programLineParser <* P.eof
