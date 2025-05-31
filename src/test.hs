import System.Console.Haskeline

-- Glavni program koji pokreće REPL petlju
main :: IO ()
main = runInputT defaultSettings petlja

-- Petlja za unos i ispis korisničkog unosa
petlja :: InputT IO ()
petlja = do
  unos <- getInputLine "% "
  case unos of
    Nothing -> return ()       -- Kraj unosa (Ctrl-D)
    Just "quit" -> return ()   -- Izlaz iz petlje
    Just tekst -> do
      outputStrLn $ "Uneseno je: " ++ tekst
      petlja