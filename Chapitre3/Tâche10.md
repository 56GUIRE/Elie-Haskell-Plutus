Voici le code Haskell demandé avec une fonction `isPalindrome` utilisant des gardes pour vérifier si une chaîne est un palindrome, et un `main` pour tester les cas spécifiés :

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

### Explication :
- La fonction `isPalindrome` prend une chaîne (`String`) et retourne un booléen (`Bool`).
- Les gardes (`|`) sont utilisées pour vérifier si la chaîne est un palindrome :
  - Si la longueur de la chaîne est 0 ou 1 (`length str <= 1`), elle est considérée comme un palindrome (`True`).
  - Si le premier caractère (`head str`) est égal au dernier caractère (`last str`), on appelle récursivement `isPalindrome` sur la sous-chaîne obtenue en supprimant le premier et le dernier caractère (`init (tail str)`).
  - Sinon (`otherwise`), la chaîne n'est pas un palindrome (`False`).
- Le `main` teste la fonction avec les chaînes `"racecar"`, `"haskell"` et `"madam"`, et affiche les résultats avec `show` pour convertir les booléens en chaînes.

### Vérification des tests :
1. **Pour `isPalindrome "racecar"`** :
   - Longueur = 7, premier (`r`) == dernier (`r`), sous-chaîne = `"aceca"`.
   - Longueur = 5, premier (`a`) == dernier (`a`), sous-chaîne = `"cec"`.
   - Longueur = 3, premier (`c`) == dernier (`c`), sous-chaîne = `"e"`.
   - Longueur = 1 ≤ 1 → `True`.
   - Résultat : `True`.
2. **Pour `isPalindrome "haskell"`** :
   - Longueur = 7, premier (`h`) ≠ dernier (`l`) → `False`.
   - Résultat : `False`.
3. **Pour `isPalindrome "madam"`** :
   - Longueur = 5, premier (`m`) == dernier (`m`), sous-chaîne = `"ada"`.
   - Longueur = 3, premier (`a`) == dernier (`a`), sous-chaîne = `"d"`.
   - Longueur = 1 ≤ 1 → `True`.
   - Résultat : `True`.

### Sortie attendue :
```
isPalindrome "racecar": True
isPalindrome "haskell": False
isPalindrome "madam": True
```

### Remarque :
- La fonction suppose que la chaîne est valide. Elle est sensible à la casse (par exemple, `"Madam"` ne serait pas considéré comme un palindrome). Si une vérification insensible à la casse est nécessaire, on pourrait convertir la chaîne en minuscules avec `map toLower` (en important `Data.Char`).
- Les fonctions `head`, `last`, `tail`, et `init` sont utilisées pour simplifier le code, mais elles peuvent échouer sur des chaînes vides. Cependant, la garde `length str <= 1` garantit que ces fonctions ne sont appelées que sur des chaînes d'au moins deux caractères.
- Pour une approche plus robuste, on pourrait ajouter des validations explicites pour les cas limites, mais l'énoncé ne l'exige pas.
