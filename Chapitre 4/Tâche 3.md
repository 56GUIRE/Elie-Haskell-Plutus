Voici le code exact que tu m'as envoyé :

```haskell
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent !"
    | grade >= 70 && grade <= 89  = "Bon travail !"
    | grade >= 50 && grade <= 69  = "Tu as réussi."
    | grade >= 0  && grade <= 49  = "Peut mieux faire."
    | otherwise                   = "Note invalide."

-- Tests
main :: IO ()
main = do
    putStrLn $ "gradeComment 95: " ++ gradeComment 95    -- Affiche "gradeComment 95: Excellent !"
    putStrLn $ "gradeComment 75: " ++ gradeComment 75    -- Affiche "gradeComment 75: Bon travail !"
    putStrLn $ "gradeComment 60: " ++ gradeComment 60    -- Affiche "gradeComment 60: Tu as réussi."
    putStrLn $ "gradeComment 45: " ++ gradeComment 45    -- Affiche "gradeComment 45: Peut mieux faire."
    putStrLn $ "gradeComment 101: " ++ gradeComment 101  -- Affiche "gradeComment 101: Note invalide."
    putStrLn $ "gradeComment (-1): " ++ gradeComment (-1) -- Affiche "gradeComment (-1): Note invalide."
```

Si tu as besoin d'autre chose, comme une explication, une modification ou des tests supplémentaires, fais-moi signe !
