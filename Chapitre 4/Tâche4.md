Voici le code exact que tu m'as envoyé dans ton dernier message :

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age = case age of
    1  -> "Premier anniversaire !"
    18 -> "Tu es adulte !"
    60 -> "Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    _  -> "Rien de spécial"

-- Tests
main :: IO ()
main = do
    putStrLn $ "specialBirthday 1: " ++ specialBirthday 1   -- Affiche "specialBirthday 1: Premier anniversaire !"
    putStrLn $ "specialBirthday 18: " ++ specialBirthday 18 -- Affiche "specialBirthday 18: Tu es adulte !"
    putStrLn $ "specialBirthday 60: " ++ specialBirthday 60 -- Affiche "specialBirthday 60: Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    putStrLn $ "specialBirthday 25: " ++ specialBirthday 25 -- Affiche "specialBirthday 25: Rien de spécial"
```

Si tu as besoin d'une explication détaillée, de modifications, ou d'autres précisions, fais-moi signe !
