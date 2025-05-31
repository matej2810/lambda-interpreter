

module Common (
  Token (..),
  Expr  (..),
  Komp  (..),
  Okruzenje,
  Evaluacija,
  pocetnoOkruzenje,
  primjeniPrimitiv,
  pronadjiUOkruzenju,
  ispis,
  greska,
  umetni
) where

import Data.HashMap.Strict as M

-- Tipovi tokena za lexer
data Token = OtvZagrada | ZatvZagrada | Rec String | LambdaZnak | Tacka | Definicija | Kraj
  deriving (Read, Show, Eq)

-- Definicija izraza 
data Expr = Varijabla String
          | Apstrakcija Expr Expr
          | Aplikacija Expr Expr
          | Veza String Expr -- veza varijable na izraz
          | Primitiv String
          deriving (Read, Eq)

-- Tip okruženja: mapa varijabli na izraze
type Okruzenje = HashMap String Expr

-- Početno okruženje s osnovnim primitivima
pocetnoOkruzenje :: Okruzenje
pocetnoOkruzenje = fromList [("show", Primitiv "show")]

-- Prikaz izraza u čitljivom obliku
instance Show Expr where
  show (Varijabla x) = x
  show (Apstrakcija x b) = "λ" ++ show x ++ "." ++ show b
  show (Aplikacija e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Veza v e) = show v ++ " := " ++ show e
  show (Primitiv s) = "<Primitiv: " ++ s ++ ">"

-- Pretraga varijable u okruženju ili pretvorba prirodnog broja u Churchov izraz
pronadjiUOkruzenju :: String -> Okruzenje -> Maybe Expr
pronadjiUOkruzenju var okr =
  if jePrirodniBroj var
    then Just (pretvoriChurch var)  
    else M.lookup var okr

-- Provjera je li string prirodan broj
jePrirodniBroj :: String -> Bool
jePrirodniBroj [] = True
jePrirodniBroj (c:cs)
  | c `elem` "0123456789" = jePrirodniBroj cs
  | otherwise = False

-- Pretvorba stringa broja u Churchov izraz
pretvoriChurch :: String -> Expr
pretvoriChurch n = Apstrakcija (Varijabla "f") (Apstrakcija (Varijabla "x") (pomocnikChurch (read n)))

pomocnikChurch :: Int -> Expr
pomocnikChurch 0 = Varijabla "x"
pomocnikChurch n = Aplikacija (Varijabla "f") (pomocnikChurch (n - 1))

-- Primjena primitivne funkcije
primjeniPrimitiv :: String -> Expr -> Okruzenje -> Evaluacija
primjeniPrimitiv "show" (Varijabla s) okr = case pronadjiUOkruzenju s okr of
  Nothing -> Left $ "Nepoznata funkcija: " ++ s
  Just e  -> Right (e, okr)
primjeniPrimitiv "show" expr okr = Right (expr, okr)
primjeniPrimitiv "None" x okr = Right (x, okr)
primjeniPrimitiv s _ _ = Left $ "Nepoznat primitiv: " ++ s

-- Tipovi za evaluaciju
type Komp = (Expr, Okruzenje)
type Evaluacija = Either String Komp

-- Ispis rezultata evaluacije
ispis :: Evaluacija -> String
ispis (Right (expr, _)) = show expr
ispis (Left err) = greska err

-- Formatiranje poruke o grešci
greska :: String -> String
greska msg = "GREŠKA: " ++ msg

-- Pomoćna funkcija za umetanje u okruženje 
umetni :: String -> Expr -> Okruzenje -> Okruzenje
umetni = M.insert
