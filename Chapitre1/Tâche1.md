-- double : multiplie un nombre par 2
double :: Int -> Int
double x = x * 2

-- increment : ajoute 1 à un nombre
increment :: Int -> Int
increment x = x + 1

-- doubleThenIncrement : applique double puis increment
doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Exemple d'utilisation
main :: IO ()
main = do
  let x = 5
  putStrLn ("Résultat de doubleThenIncrement " ++ show x ++ " = " ++ show (doubleThenIncrement x))
