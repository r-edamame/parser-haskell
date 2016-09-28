
module Edamame.Parser.JSON
  (
    parseJSON,
    JSONValue(..)
  ) where

import Edamame.Parser
import Data.Array.IArray
import Data.List

data JSONValue =
  JSONString String |
  JSONNumber Double |
  JSONObject [(String,JSONValue)] |
  JSONArray (Array Int JSONValue) |
  JSONBool Bool |
  JSONNull
  deriving (Eq)

instance Show JSONValue where
  show (JSONString str) = str
  show (JSONNumber num) = show num
  show (JSONObject obj) = "{}"
  show (JSONArray arr) = "[" ++ (concat $ intersperse "," $ map show $ elems arr) ++ "]"
  show (JSONBool b) = show b
  show JSONNull = "null"

notQuotation :: Parser Char
notQuotation = sat $ not . (=='\"')

innerQuotation :: Parser Char
innerQuotation = string "\\\"" >> return '\"'

escaped :: Parser Char
escaped =
  (string "\\\"" >> return '\"') <||>
  (string "\\\\" >> return '\\')

expo :: Parser String
expo = (\x y z->x : (y ++ z)) <$> (char 'e' <||> char 'E') <*> (string "+" <||> string "-" <||> return "") <*> many1 digit

jsonstring :: Parser JSONValue
jsonstring = do
  char '\"'
  xs <- many (escaped <||> notQuotation)
  char '\"'
  return $ JSONString xs

jsonnum :: Parser JSONValue
jsonnum = do
  sig <- string "-" <||> return ""
  int <- string "0" <||> many1 digit
  dec <- ((:) <$> char '.' <*> many1 digit) <||> return ""
  e <- expo <||> return ""
  return $ JSONNumber $ read (sig++(int++(dec++e)))

jsonbool :: Parser JSONValue
jsonbool = JSONBool <$> token ((string "true" >> return True) <||> (string "false" >> return False))

jsonnull :: Parser JSONValue
jsonnull = token (string "null" >> return JSONNull)

kv :: Parser (String,JSONValue)
kv = do
  JSONString key <- token jsonstring
  token $ char ':'
  val <- jsonvalue
  return (key,val)

jsonobj :: Parser JSONValue
jsonobj = do
  token $ char '{'
  xs <- many (kv >>= \x-> token (char ',') >> return x)
  lst <- (kv >>= return . return) <||> return []
  token $ char '}'
  return $ JSONObject $ xs++lst


jsonarr :: Parser JSONValue
jsonarr = do
  token $ char '['
  xs <- many (jsonvalue >>= \v-> token (char ',') >> return v)
  lst <- (jsonvalue >>= return . return)  <||> return []
  token $ char ']'
  let list = xs++lst
      len = length list
  return $ JSONArray $ listArray (0,len-1) list

jsonvalue :: Parser JSONValue
jsonvalue = token (jsonstring <||> jsonnum <||> jsonobj  <||> jsonarr <||> jsonbool <||> jsonnull)

parseJSON :: String -> Maybe JSONValue
parseJSON inp = fst <$> parse jsonvalue inp
