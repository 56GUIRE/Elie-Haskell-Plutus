HC6T2 : Suite de Fibonacci (récursif)
```haskell
-- Fonction récursive pour calculer le nième nombre de Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
    | n < 0     = error "Erreur : l'index de Fibonacci n'est pas défini pour les nombres négatifs."
    | n > 30    = error "Erreur : index trop grand, veuillez entrer un nombre <= 30."
    | otherwise = fib (n - 1) + fib (n - 2)

-- Fonction principale
main :: IO ()
main = do
    putStrLn "Entrez un index non négatif (max 30) pour calculer le nombre de Fibonacci :"
    input <- getLine
    let n = read input :: Integer
    let result = fib n
    putStrLn $ "Le nombre de Fibonacci à l'index " ++ input ++ " est : " ++ show result
    `catch` \e -> putStrLn (show (e :: SomeException))
```

### Explications
1. **Fonction `fib`** :
   - Cas de base : `fib 0 = 0` et `fib 1 = 1`.
   - Récursion : `fib n = fib (n - 1) + fib (n - 2)` pour `n ≥ 2`.
   - Vérifications : Rejette les nombres négatifs et limite à 30 pour éviter des calculs trop longs (la récursion naïve de Fibonacci devient exponentielle, et au-delà de 30, elle risque un timeout ou un débordement).

2. **Fonction `main`** :
   - Demande un index utilisateur via `getLine`.
   - Convertit l'entrée en `Integer` avec `read`.
   - Calcule et affiche le résultat.
   - Utilise `catch` pour gérer les erreurs (entrées invalides ou exceptions).

### Exemples d'exécution
- Entrée : `5`
  ```
  Entrez un index non négatif (max 30) pour calculer le nombre de Fibonacci :
  Le nombre de Fibonacci à l'index 5 est : 5
  ```
  (La suite est : 0, 1, 1, 2, 3, 5, ...)

- Entrée : `31`
  ```
  Entrez un index non négatif (max 30) pour calculer le nombre de Fibonacci :
  *** Exception: user error (Erreur : index trop grand, veuillez entrer un nombre <= 30.)
  ```

- Entrée : `abc`
  ```
  Entrez un index non négatif (max 30) pour calculer le nombre de Fibonacci :
  *** Exception: Prelude.read: no parse
  ```

### Remarques
- **Limite de 30** : La récursion naïve de Fibonacci est très lente pour de grands nombres (complexité O(2^n)). Une limite de 30 est raisonnable pour éviter les timeouts dans des environnements en ligne avec des contraintes de temps (par exemple, 7 secondes). Pour des valeurs plus grandes, une implémentation itérative ou avec mémoïsation serait préférable, mais cela compliquerait le code.
- **Compilation locale** : Si vous utilisez GHC, sauvegardez dans `Fibonacci.hs`, compilez avec `ghc Fibonacci.hs`, puis exécutez avec `./Fibonacci`.
- **Environnement en ligne** : Testez dans votre éditeur (comme CompilerBerry) en cliquant sur "RUN".

Testez ce code et dites-moi si vous rencontrez des problèmes ou si vous voulez une version optimisée !
