HC5T3 : Vérifier la présence de majuscules
Voici le code en Haskell qui utilise la fonction `any` pour vérifier si une liste de mots contient au moins un mot commençant par une majuscule, avec un `main` pour tester :

```haskell
import Data.Char (isUpper)

-- Fonction pour vérifier si un mot commence par une majuscule
hasCapital :: [String] -> Bool
hasCapital words = any (\w -> not (null w) && isUpper (head w)) words

-- Fonction principale
main :: IO ()
main = do
    let testList1 = ["hello", "World", "haskell", "Code"]
    let testList2 = ["hello", "world", "haskell"]
    print $ hasCapital testList1  -- Affiche True
    print $ hasCapital testList2  -- Affiche False
```

### Explications :
- `import Data.Char (isUpper)` importe la fonction `isUpper` pour vérifier si un caractère est une majuscule.
- La fonction `hasCapital` prend une liste de mots (`[String]`) et utilise `any` pour vérifier si au moins un mot satisfait la condition :
  - `\w -> not (null w) && isUpper (head w)` est une fonction anonyme qui vérifie si un mot `w` n'est pas vide (`not (null w)`) et si son premier caractère est une majuscule (`isUpper (head w)`).
- Dans le `main` :
  - `testList1` contient des mots dont certains commencent par une majuscule ("World", "Code"), donc `hasCapital testList1` retourne `True`.
  - `testList2` ne contient aucun mot commençant par une majuscule, donc `hasCapital testList2` retourne `False`.
- `print` affiche les résultats dans la console.

Ce code est idiomatique, concis et peut être exécuté dans GHC pour voir les résultats.
