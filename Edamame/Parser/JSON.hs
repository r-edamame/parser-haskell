
module Edamame.Parser.JSON (parseJSON) where

import Edamame.Parser
import Data.Array.IArray

data JSONValue =
  JSONString String |
  JSONNumber Double |
  JSONObject [(String,JSONValue)] |
  --JSONArray (Array Int JSONValue) |
  JSONArray [JSONValue] |
  JSONBool Bool |
  JSONNull
  deriving (Show,Eq)

type JObj = [(String,JSONValue)]
type JArr = Array Int JSONValue

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
  char '{'
  xs <- many (kv >>= \x-> token (char ',') >> return x)
  lst <- (kv >>= return . return) <||> return []
  char '}'
  return $ JSONObject $ xs++lst


jsonarr :: Parser JSONValue
jsonarr = do
  char '['
  xs <- many (jsonvalue >>= \v-> token (char ',') >> return v)
  lst <- (jsonvalue >>= return . return)  <||> return []
  char ']'
  return $ JSONArray $ xs++lst 

jsonvalue :: Parser JSONValue
jsonvalue = token (jsonstring <||> jsonnum <||> jsonobj  <||> jsonarr <||> jsonbool <||> jsonnull)

parseJSON = jsonvalue
