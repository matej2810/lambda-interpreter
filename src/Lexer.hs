module Lexer (
    tokenize
,   head_words
) where 

import Common

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs)
  | x == 'λ' = LambdaZnak : tokenize xs
  | x == '\\' = LambdaZnak : tokenize xs
  | x == '(' = OtvZagrada : tokenize xs
  | x == ')' = ZatvZagrada : tokenize xs
  | x == '.' = Tacka : tokenize xs
  | x == ':' && not (null xs) && head xs == '=' = Definicija : tokenize (tail xs)
  | x == ' ' = tokenize xs
  | x == '-' && not (null xs) && head xs == '-' = []  
  | otherwise = tokenize_word (x:xs)


tokenize_word :: String -> [Token]
tokenize_word (x:xs) = let ans = (head_words (x:xs)) in Rec (fst ans) : (tokenize (snd ans))


head_words :: String -> (String, String)
head_words "" = ("","")
head_words (x:xs)
  | x == ' ' = ("", x:xs)
  | x == '.' = ("", x:xs)
  | x == '(' = ("", x:xs)
  | x == ')' = ("", x:xs)
  | x == ':' = ("", x:xs)
  | x == '\\'= ("", x:xs)
  | x == 'λ' = ("", x:xs)
  | otherwise = let ans = (head_words xs) in (x : fst ans, snd ans)