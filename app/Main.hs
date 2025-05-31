import System.Console.Haskeline
import System.IO
import Control.Monad
import qualified Evaluator as Evalr
import qualified Lexer as Lexr
import qualified Parser as Parsr
import qualified Common as Cmn

-- Ulazna točka programa, odmah pokreće REPL s početnim okruženjem
main :: IO ()
main = petljaREPL Cmn.pocetnoOkruzenje

-- REPL petlja - čeka korisnički unos i obrađuje ga
petljaREPL :: Cmn.Okruzenje -> IO ()
petljaREPL env = runInputT defaultSettings (petljaUnosa env)

-- Petlja unosa u REPL-u
petljaUnosa :: Cmn.Okruzenje -> InputT IO ()
petljaUnosa env = do
  unos <- getInputLine "λ> "
  case unos of
    Nothing -> return ()
    Just linija -> if jeKraj linija
                   then return ()
                   else do
                     let rezultat = case Parsr.parse (Lexr.tokenize linija) of
                           Left err -> Left err
                           Right (ast, _) -> Evalr.normalize (ast, env)
                     outputStrLn (formatirajRezultat rezultat)
                     case rezultat of
                       Left _ -> petljaUnosa env
                       Right (_, novoEnv) -> petljaUnosa novoEnv


-- Evaluacija jedne linije unosa
evaluirajLiniju :: String -> Cmn.Okruzenje -> Either String (Cmn.Expr, Cmn.Okruzenje)
evaluirajLiniju ulaz okruzenje = case Parsr.parse (Lexr.tokenize ulaz) of
  Left err -> Left err
  Right (ast, _) -> Evalr.normalize (ast, okruzenje)



-- Provjera je li korisnik unio naredbu za izlaz
jeKraj :: String -> Bool
jeKraj str = str `elem` ["quit", "exit", ":q"]

-- Formatiranje rezultata evaluacije za ispis
formatirajRezultat :: Either String (Cmn.Expr, Cmn.Okruzenje) -> String
formatirajRezultat (Left greska) = "Greška: " ++ greska
formatirajRezultat (Right (izraz, _)) = show izraz
