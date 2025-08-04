Votre demande est identique à une précédente, où j'ai fourni un code Haskell pour une fonction `isPalindrome` avec des gardes et des tests pour `"racecar"`, `"haskell"`, et `"madam"`. Voici à nouveau le code, inchangé, car il répond exactement à vos exigences :

```haskell
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1           = True
    | head str == last str      = isPalindrome (init (tail str))
    | otherwise                 = False

main :: IO ()
main = do
    putStrLn $ "isPalindrome \"racecar\": " ++ show (isPalindrome "racecar")
    putStrLn $ "isPalindrome \"haskell\": " ++ show (isPalindrome "haskell")
    putStrLn $ "isPalindrome \"madam\": " ++ show (isPalindrome "madam")
```

### Explication (répétée pour clarté) :
- La fonction `isPalindrome` prend une chaîne (`String`) et retourne un booléen (`Bool`).
- Les gardes (`|`) vérifient :
  - Si la longueur de la chaîne est 0 ou 1 (`length str <= 1`), retourne `True`.
  - Si le premier caractère (`head str`) est égal au dernier (`last str`), appelle récursivement `isPalindrome` sur la sous-chaîne sans le premier et le dernier caractère (`init (tail str)`).
  - Sinon (`otherwise`), retourne `False`.
- Le `main` teste avec `"racecar"`, `"haskell"`, et `"madam"`, en affichant les résultats.

### Sortie attendue :
```
isPalindrome "racecar": True
isPalindrome "haskell": False
isPalindrome "madam": True
```

### Remarque :
- Comme mentionné précédemment, la fonction est sensible à la casse (par exemple, `"Madam"` ne serait pas un palindrome). Une conversion en minuscules peut être ajoutée si nécessaire (en utilisant `map toLower` avec `import Data.Char`).
- La garde `length str <= 1` protège contre les erreurs sur `head`, `last`, `tail`, et `init` pour les chaînes vides ou d'un seul caractère.
- Si vous avez des modifications ou des exigences supplémentaires (par exemple, insensible à la casse ou gestion d'espaces), veuillez préciser !
