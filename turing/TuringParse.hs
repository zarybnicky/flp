-- Textová analýza Turingova stroje na vstupu
-- Využívá se knihovny Parsec

{-# LANGUAGE RecordWildCards #-}
module TuringParse (parseTM)  where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Bool (bool)
import Text.Parsec (Parsec, ParseError, parse,
        newline, alphaNum, string, char, satisfy, sepBy1, endBy, many1)
import Text.Parsec.String (Parser)

import TuringData


-- Převod TM z textu do vnitřní reprezentace
-- Funkce 'parse' aplikuje parser na řetězec a 'left show' převede chybový výsledek
-- na chybovou hlášku z typu 'Err TMachine'
parseTM :: String -> Err TMachine
parseTM = validate <=< left show . parse tmParser ""


-- Analýza celého TM
tmParser :: Parser TMachine
tmParser =
  TM <$> stateListP <* newline <*> alphabetP <* newline <*> stateP <* newline <*>
  stateP <*
  newline <*>
  transitionsP


-- seznam stavů
stateListP :: Parser [TState]
stateListP = sepBy1 stateP comma

stateP :: Parser TState
stateP = many1 alphaNum


-- abeceda
alphabetP :: Parser [TSymbol]
alphabetP = many1 symbP

symbP :: Parser TSymbol  -- cokoliv, co není mezi vyjmenovanými znaky
symbP = satisfy (`notElem` " ,<>\n\t")


-- seznam pravidel, na každém řádku jedno
transitionsP :: Parser [Transition]
transitionsP = endBy transP newline

transP :: Parser Transition
transP =
  Trans <$> stateP <* comma <*> symbP <* comma <*> stateP <* comma <*> actP
  where
    actP = ALeft <$ char '<' <|> ARight <$ char '>' <|> AWrite <$> symbP


-- oddělovač
comma :: Parser Char
comma = char ','

-- Validační funkce: všechny stavy musí být v seznamu stavů a všechny symboly v abecedě
validate :: TMachine -> Err TMachine
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
