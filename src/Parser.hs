module Parser (
    parse
) where

import Common

-- Glavna funkcija za parsiranje: prima listu tokena i vraća AST i ostatak tokena
parse :: [Token] -> Either String (Expr, [Token])
parse x = parse_aplikacija (parse_atom x)

-- Parsiranje aplikacija (lijevo asocijativno)
parse_aplikacija :: Either String (Expr, [Token]) -> Either String (Expr, [Token])
parse_aplikacija (Left s) = Left s
parse_aplikacija (Right (prvi, ostatak)) =
  if null ostatak
    then Right (prvi, [])
    else parse_atom ostatak >>= \(sljedeci, ostatak2) ->
           parse_aplikacija (Right (Aplikacija prvi sljedeci, ostatak2))

-- Provjera je li token otvarajuća zagrada
je_zagrada :: [Token] -> Bool
je_zagrada (x:_) = x == OtvZagrada
je_zagrada _ = False

-- Provjera je li token lambda znak (apstrakcija)
je_apstrakcija :: [Token] -> Bool
je_apstrakcija (x:_) = x == LambdaZnak
je_apstrakcija _ = False

-- Provjera je li token riječ (varijabla)
je_rijec :: [Token] -> Bool
je_rijec ((Rec _):_) = True
je_rijec _ = False

-- Provjera je li definicija (npr. "x := ...")
je_definicija :: [Token] -> Bool
je_definicija ((Rec _):Definicija:_) = True
je_definicija _ = False

-- Dispatcher za parsiranje atomskih izraza
parse_atom :: [Token] -> Either String (Expr, [Token])
parse_atom x
  | je_definicija x = parse_definiciju x -- definicija se ne može primijeniti
  | je_zagrada x = parse_zagrade x       -- izraz u zagradama
  | je_apstrakcija x = parse_apstrakciju x
  | je_rijec x = parse_rijec x
  | null x = Right (Primitiv "None", [])
  | otherwise = Left "Nepoznat izraz"

-- Parsiranje definicije (veza)
parse_definiciju :: [Token] -> Either String (Expr, [Token])
parse_definiciju (var:_:def) =
  parse def >>= \(expr, ostatak) -> Right (Veza (token_u_string var) expr, ostatak)
parse_definiciju _ = Left "Neispravna sintaksa definicije"

-- Parsiranje riječi (varijable)
parse_rijec :: [Token] -> Either String (Expr, [Token])
parse_rijec ((Rec x):xs) = Right (Varijabla x, xs)
parse_rijec _ = Left "Greška pri parsiranju riječi"

-- Parsiranje izraza u zagradama
parse_zagrade :: [Token] -> Either String (Expr, [Token])
parse_zagrade x = do
  (tijelo, ostatak) <- izvuci_iz_zagrada x
  if je_apstrakcija tijelo || je_zagrada tijelo || je_rijec tijelo
    then do
      (expr, _) <- parse tijelo  
      return (expr, ostatak)
    else Left "Ne mogu parsirati izraz u zagradama"


-- Pomoćna funkcija za izdvajanje tokena unutar vanjskih zagrada
helper :: [Token] -> Int -> Either String ([Token], [Token])
helper [] n = if n == 0 then Right ([], []) else Left "Neusklađene zagrade"
helper (x:xs) n
  | n == 0 = Right ([], x:xs)
  | x == OtvZagrada = do
      (inside, rest) <- helper xs (n + 1)
      Right (x : inside, rest)
  | x == ZatvZagrada && n == 1 = Right ([], xs)
  | x == ZatvZagrada = do
      (inside, rest) <- helper xs (n - 1)
      Right (x : inside, rest)
  | otherwise = do
      (inside, rest) <- helper xs n
      Right (x : inside, rest)

-- Izdvajanje izraza unutar vanjskih zagrada
izvuci_iz_zagrada :: [Token] -> Either String ([Token], [Token])
izvuci_iz_zagrada (OtvZagrada:xs) = helper xs 1
izvuci_iz_zagrada _ = Left "Nije izraz u zagradama"

-- Parsiranje lambda apstrakcije
parse_apstrakciju :: [Token] -> Either String (Expr, [Token])
parse_apstrakciju (LambdaZnak:x:Tacka:xs) =
  if je_rijec [x]
    then do
      (expr, rest) <- parse xs
      return (Apstrakcija (Varijabla (token_u_string x)) expr, rest)
    else Left "Pokušaj kreiranja apstrakcije nad ne-riječju"
parse_apstrakciju _ = Left "Neispravan lambda izraz"

-- Pretvaranje tokena u string (za Rec)
token_u_string :: Token -> String
token_u_string (Rec x) = x
token_u_string _ = error "Očekivan token tip Rec"
