HC5T5 : Application partielle
Voici le code en Haskell qui définit une fonction `multiplierByFive` utilisant l'a :

```haskell
-- Fonction utilisant l'application partielle pour multiplier par 5
multiplierByFive :: Int -> Int
multiplierByFive = (* 5)

-- Fonction principale
main :: IO ()
main = do
    print $ multiplierByFive 3  -- Affiche 15
    print $ multiplierByFive 10 -- Affiche 50
```

### Explications :
- La fonction `multiplierByFive` est définie en utilisant l'application partielle de l'opérateur `*`. En écrivant `(* 5)`, on fixe le premier argument de `*` à 5, ce qui crée une fonction qui prend un entier et le multiplie par 5.
- Dans le `main`, on teste la fonction avec deux valeurs :
  - `multiplierByFive 3` retourne `15` (car 3 * 5 = 15).
  - `multiplierByFive 10` retourne `50` (car 10 * 5 = 50).
- `print` affiche les résultats dans la console.

Ce code est idiomatique, concis et peut être exécuté dans GHC pour voir les résultats.
