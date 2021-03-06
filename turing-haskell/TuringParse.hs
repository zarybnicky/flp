{-# LANGUAGE RecordWildCards #-}

-- | Textová analýza Turingova stroje na vstupu
-- Využívá se knihovny Parsec

module TuringParse (parseTM)  where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (alphaNum, char, endBy, many1, newline, parse, satisfy, sepBy1)
import Text.Parsec.String (Parser)

import TuringData


-- Převod TM z textu do vnitřní reprezentace
-- Funkce 'parse' aplikuje parser na řetězec a 'left show' převede chybový výsledek
-- na chybovou hlášku z typu 'Either String TMachine'
parseTM :: String -> Either String TMachine
parseTM = validate <=< left show . parse tmParser ""

-- Analýza celého TM
tmParser :: Parser TMachine
tmParser = do
  states <- sepBy1 stateP (char ',')
  _ <- newline
  alphabet <- many1 symbP
  _ <- newline
  start <- stateP
  _ <- newline
  end <- stateP
  _ <- newline
  transRules <- endBy transP newline
  pure TM {..}

stateP :: Parser TState
stateP = many1 alphaNum

symbP :: Parser TSymbol  -- cokoliv, co není mezi vyjmenovanými znaky
symbP = satisfy (`notElem` " ,<>\n\t")

transP :: Parser Transition
transP = do
  fromState <- stateP
  _ <- char ','
  fromSym <- symbP
  _ <- char ','
  toState <- stateP
  _ <- char ','
  toAction <- ALeft <$ char '<' <|> ARight <$ char '>' <|> AWrite <$> symbP
  pure Trans {..}

-- Validační funkce: všechny stavy musí být v seznamu stavů a všechny symboly v abecedě
validate :: TMachine -> Either String TMachine
validate tm@TM {..} =
  if allOK
    then Right tm
    else Left "invalid TM"
  where
    allOK =
      start `elem` states &&
      end `elem` states &&
      all ((`elem` states) . fromState) transRules &&
      all ((`elem` alphabet) . fromSym) transRules &&
      all ((`elem` states) . toState) transRules &&
      all (`elem` alphabet) [c | AWrite c <- map toAction transRules]
