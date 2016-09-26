
module Edamame.Parser where

import Data.Char
import Data.Monoid

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f p = Parser $ \inp -> case parse p inp of
                        Just (x,rem) -> Just (f x, rem)
                        Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \inp -> Just (x,inp)
  (Parser pf) <*> p = Parser $
    \inp -> case pf inp of
      Just (f,rem) -> parse (fmap f p) rem
      Nothing -> Nothing

instance Monad Parser where
  return = pure
  p >>= pf = Parser $ \inp -> case parse p inp of
    Just (x,rem) -> parse (pf x) rem
    Nothing -> Nothing

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = Parser $ \inp -> getFirst $ (First $ parse p inp) <> (First $ parse q inp)

failure :: Parser a
failure = Parser $ \inp -> Nothing

item :: Parser Char
item = Parser $ \inp -> case inp of
                  "" -> Nothing
                  (x:xs) -> Just (x,xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = (:) <$> char x <*> string xs

many :: Parser a -> Parser [a]
many p = many1 p <||> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p >>= \v-> space >> return v
