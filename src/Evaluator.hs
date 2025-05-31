module Evaluator (
  evaliraj,
  primjeni,
  zamijeni,
  omotaj,
  vrijednostVarijable,
  normalize
) where

import Common

-- Evaluacija izraza u danom okruženju
evaliraj :: Komp -> Evaluacija
evaliraj (a@(Varijabla v), okr) = Right (a, okr)
evaliraj (a@(Apstrakcija v b), okr) = Right (a, okr)
evaliraj (p@(Primitiv s), okr) = Right (p, okr)
evaliraj a@((Aplikacija e1 e2), okr) = primjeni a
evaliraj ((Veza s e), okr) =
  if isKombinator (e, okr)
    then evaliraj (e, okr) >>= \(x, _) -> case pronadjiUOkruzenju s okr of
      Nothing -> Right (x, umetni s x okr)
      Just _  -> Left $ s ++ " je već definiran"
    else Left $ "Izraz: " ++ show e ++ " sadrži nedefinirane varijable"

-- Primjena funkcije na argument
primjeni :: Komp -> Evaluacija
primjeni ((Aplikacija e1@(Apstrakcija x b) e2), okr) = zamijeni x b e2 okr >>= evaliraj
primjeni ((Aplikacija e1@(Varijabla x) e2), okr) = case pronadjiUOkruzenju x okr of
  Nothing -> Left $ "Nema definicije za varijablu: " ++ show e1
  Just expr -> primjeni ((Aplikacija expr e2), okr)
primjeni ((Aplikacija e1@(Aplikacija a b) e2), okr) =
  evaliraj (e1, okr) >>= \(exp, _) -> primjeni ((Aplikacija exp e2), okr)
primjeni ((Aplikacija (Primitiv s) e), okr) = primjeniPrimitiv s e okr
primjeni (x, _) = Left $ "Pokušaj primjene izraza koji nije funkcija: " ++ show x

-- Zamjena varijable u tijelu izraza
zamijeni :: Expr -> Expr -> Expr -> Okruzenje -> Evaluacija
zamijeni (Varijabla ime) tijelo@(Varijabla x) vrijednost okr =
  if x == ime then Right (vrijednost, okr) else Right (tijelo, okr)

-- Lokalno skopiranje: ako su varijable iste, ne zamjenjuj podstablo
zamijeni (Varijabla ime) tijelo@(Apstrakcija x b) vrijednost okr =
  if vrijednostVarijable x == ime
    then Right (tijelo, okr)
    else omotaj Apstrakcija (Right (x, okr)) (zamijeni (Varijabla ime) b vrijednost okr)

zamijeni (Varijabla ime) tijelo@(Aplikacija x y) vrijednost okr =
  omotaj Aplikacija (zamijeni (Varijabla ime) x vrijednost okr) (zamijeni (Varijabla ime) y vrijednost okr)

zamijeni bad@(Apstrakcija x y) _ _ _ = Left $ "Ne može se vezati apstrakcija s λ: " ++ show bad
zamijeni bad@(Aplikacija x y) _ _ _ = Left $ "Ne može se vezati aplikacija s λ: " ++ show bad

-- Omotavanje rezultata evaluacije u konstruktor tipa
omotaj :: (Expr -> Expr -> Expr) -> Evaluacija -> Evaluacija -> Evaluacija
omotaj tc (Right (e1, okr)) (Right (e2, _)) = Right (tc e1 e2, okr)
omotaj _ x y = Left $ "Ne može se omotati konstruktor tipa, došlo je do greške: " ++ show x ++ " i " ++ show y

-- Izvlačenje imena varijable iz izraza
vrijednostVarijable :: Expr -> String
vrijednostVarijable (Varijabla ime) = ime

-- Provjera da li je izraz definiran u smislu svih varijabli
isKombinator :: Komp -> Bool
isKombinator ((Apstrakcija (Varijabla s) e), okr) = isKombinator (e, umetni s (Varijabla s) okr)
isKombinator ((Aplikacija e1 e2), okr) = isKombinator (e1, okr) && isKombinator (e2, okr)
isKombinator ((Varijabla v), okr) = provjeriVar (pronadjiUOkruzenju v okr)
isKombinator ((Primitiv s), okr) = provjeriVar (pronadjiUOkruzenju s okr)
isKombinator ((Veza s e), okr) = isKombinator (e, okr)

-- Provjera da li je varijabla definirana
provjeriVar :: Maybe Expr -> Bool
provjeriVar Nothing = False
provjeriVar (Just _) = True

normalize :: Komp -> Evaluacija
normalize komp = do
  (e', okr') <- evaliraj komp
  if e' == fst komp
    then Right (e', okr')  -- nema promjene, izraz je u normalnoj formi
    else normalize (e', okr')