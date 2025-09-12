-- Fonction qui calcule la somme des éléments d'une liste avec foldr
sumList :: [Integer] -> Integer
sumList xs = foldr (+) 0 xs

-- Fonction principale
main :: IO ()
main = do
    putStrLn "Entrez une liste d'entiers séparés par des espaces (ex: 1 2 3 4) :"
    input <- getLine
    let numbers = map read (words input) :: [Integer]
    let result = sumList numbers
    putStrLn $ "La somme des éléments est : " ++ show result
    `catch` \e -> putStrLn ("Erreur : " ++ show (e :: SomeException))
