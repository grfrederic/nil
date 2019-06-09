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
  _ <- P.many (P.single ' ' <|> P.single '\t')
  return ()


-- simpler space, _does_ consume newline
spaceOrNL :: Parser ()
spaceOrNL = do
  _ <- P.many (P.single ' ' <|> P.single '\t' <|> P.single '\n')
  return ()


-- comment
comment :: Parser ()
comment =  L.skipLineComment (T.pack "--") 
       <|> L.skipLineComment (T.pack "<nic tu nie ma, nie ma czego parsować>")


-- basic types
konstruktorParser :: Parser KonstruktorS
konstruktorParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . KonstruktorS $ s0:rs


variableParser :: Parser VariableS
variableParser = do
  s0 <- P.single '~'
  s1 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . VariableS $ s0:s1:rs


tagParser :: Parser TagS
tagParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . TagS $ s0:rs


typKonstruktorParser :: Parser TypKonstruktorS
typKonstruktorParser = do
  s0 <- C.upperChar
  rs <- P.many C.alphaNumChar
  return . TypKonstruktorS $ s0:rs


typVariableParser :: Parser TypVariableS
typVariableParser = do
  s0 <- P.single '~'
  s1 <- C.upperChar
  rs <- P.many C.alphaNumChar
  return . TypVariableS $ s0:s1:rs


relationParser :: Parser RelationS
relationParser = do
  s0 <- C.lowerChar
  rs <- P.many C.alphaNumChar
  return . RelationS $ s0:rs


-- complex types
daseinParserA :: Parser DaseinS
daseinParserA =  fmap DaseinVS variableParser
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
      return $ DaseinKS k ds

    record = do
      k <- konstruktorParser
      _ <- space >> P.single '{' >> space
      tds <- tagDasein `P.sepBy` (space >> P.single ',' >> space)
      _ <- space >> P.single '}'
      return $ DaseinRS k tds 

    tagDasein = do
      t <- tagParser
      _ <- space >> P.single ':' >> space
      d <- daseinParserA
      return (t, d)


daseinParserB :: Parser DaseinS
daseinParserB =  fmap DaseinVS variableParser
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
      return $ DaseinKS k []

    record = do
      k <- konstruktorParser
      _ <- space >> P.single '{' >> space
      tds <- tagDasein `P.sepBy` (space >> P.single ',' >> space)
      _ <- space >> P.single '}'
      return $ DaseinRS k tds 

    tagDasein = do
      t <- tagParser
      _ <- space >> P.single ':' >> space
      d <- daseinParserA
      return (t, d)


typDaseinParserA :: Parser TypDaseinS
typDaseinParserA =  fmap TypDaseinVS typVariableParser
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
      return $ TypDaseinKS t ts


typDaseinParserB :: Parser TypDaseinS
typDaseinParserB =  fmap TypDaseinVS typVariableParser
                <|> bracketed
                <|> simple
  where
    bracketed = do
      t <- P.single '(' >> space >> typDaseinParserA
      _ <- space >> P.single ')'
      return t

    simple = do
      t <- typKonstruktorParser
      return $ TypDaseinKS t []


termParser :: Parser TermS
termParser = do
  r <- relationParser
  _ <- space
  ds <- daseinParserB `P.sepEndBy1` space
  return $ TermS r ds


bedingungParser :: Parser BedingungS
bedingungParser = fmap BedingungS $ termParser `P.sepBy` comma
  where
    comma = space >> P.single ',' >> spaceOrNL


konsequenzParserJ :: Parser KonsequenzS
konsequenzParserJ =  fmap (KonsequenzS . Just) termParser

konsequenzParserN :: Parser KonsequenzS
konsequenzParserN =  return $ KonsequenzS Nothing

konsequenzParser :: Parser KonsequenzS
konsequenzParser =  P.try konsequenzParserJ <|> konsequenzParserN


-- program line
konstruktorTDParser :: Parser TypDefinitionS
konstruktorTDParser =  P.try normal -- a : A B C -> D
                   <|> short        -- a : A
  where
    normal = do
      k <- konstruktorParser
      _ <- space >> P.single ':' >> space
      ts <- typDaseinParserB `P.sepEndBy` space
      _ <- space >> C.string (T.pack "->") >> space
      rt <- typDaseinParserA
      return $ KonstruktorTDS k ts rt

    short = do
      k <- konstruktorParser
      _ <- space >> P.single ':' >> space
      rt <- typDaseinParserA
      return $ KonstruktorTDS k [] rt


recordTDParser :: Parser TypDefinitionS
recordTDParser = do -- book : {id: Int, author: String} -> Book
  k <- konstruktorParser
  _ <- space >> P.single ':' >> space >> P.single '{'
  ttds <- tagTypDasein `P.sepBy` (space >> P.single ',' >> space)
  _ <- space >> P.single '}' >> space >> C.string (T.pack "->") >> space
  rt <- typDaseinParserA
  return $ RecordTDS k ttds rt
    where
      tagTypDasein = do
        t <- tagParser
        _ <- space >> P.single ':' >> space
        td <- typDaseinParserA
        return (t, td)


relationTDParser :: Parser TypDefinitionS
relationTDParser = do
  r <- relationParser
  _ <- space >> P.single '?' >> space
  ts <- typDaseinParserB `P.sepEndBy1` space
  return $ RelationTDS r ts


typDefinitionParser :: Parser TypDefinitionS
typDefinitionParser =  P.try konstruktorTDParser
                   <|> P.try recordTDParser
                   <|> relationTDParser


klauselParser :: Parser KlauselS
klauselParser =  P.try full    -- k1 <- b1, ..., bn
             <|> P.try shortQ  -- b1, ..., bn?
             <|> P.try shortS  -- k1
  where
    full = do
      k <- konsequenzParser
      _ <- space >> C.string (T.pack "<-") >> space
      b <- bedingungParser
      return $ KlauselS k b

    shortS = do
      k <- konsequenzParserJ
      return $ KlauselS k (BedingungS [])

    shortQ = do
      b <- bedingungParser
      _ <- space >> P.single '?' >> space
      return $ KlauselS (KonsequenzS Nothing) b


-- program
programmzeileParser :: Parser (Maybe (Either TypDefinitionS KlauselS))
programmzeileParser =  do
  _ <- space
  l <- fmap (Just . Left) (P.try typDefinitionParser)
   <|> fmap (Just . Right) (P.try klauselParser)
   <|> return Nothing
  _ <- space >> P.optional (P.try comment)
  _ <- P.single '\n'
  return l


programmParser :: Parser ProgrammS
programmParser = fmap (ProgrammS . catMaybes) $ P.many programmzeileParser <* P.eof
