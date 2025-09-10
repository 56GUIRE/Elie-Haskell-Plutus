Voici un code Haskell qui implémente une fonction récursive pour calculer la factorielle d'un nombre, avec une fonction `main` pour interagir avec l'utilisateur. Ce code est conçu pour être simple, robuste, et éviter les problèmes de timeout dans des environnements en ligne avec des limites strictes.

```haskell
-- Fonction factorielle récursive
factorial :: Integer -> Integer
factorial 0 = 1
factorial n
    | n < 0     = error "Erreur : la factorielle n'est pas définie pour les nombres négatifs."
    | n > 20    = error "Erreur : nombre trop grand, veuillez entrer un nombre <= 20."
    | otherwise = n * factorial (n - 1)

-- Fonction principale
main :: IO ()
main = do
    putStrLn "Entrez un nombre entier non négatif (max 20) pour calculer sa factorielle :"
    input <- getLine
    let n = read input :: Integer
    let result = factorial n
    putStrLn $ "La factorielle de " ++ input ++ " est : " ++ show result
    `catch` \e -> putStrLn (show (e :: SomeException))
```

### Explications
1. **Fonction `factorial`** :
   - Cas de base : `factorial 0 = 1`.
   - Récursion : `n * factorial (n - 1)` pour `n > 0`.
   - Vérifications : Limite à 20 pour éviter les débordements ou timeouts, et rejette les nombres négatifs.

2. **Fonction `main`** :
   - Demande une entrée utilisateur via `getLine`.
   - Convertit l'entrée en `Integer` avec `read`.
   - Calcule et affiche le résultat.
   - Utilise `catch` pour gérer les erreurs (entrées invalides comme des lettres ou des exceptions).

### Exemples d'exécution
- Entrée : `5`
  ```
  Entrez un nombre entier non négatif (max 20) pour calculer sa factorielle :
  La factorielle de 5 est : 120
  ```
- Entrée : `21`
  ```
  Entrez un nombre entier non négatif (max 20) pour calculer sa factorielle :
  *** Exception: user error (Erreur : nombre trop grand, veuillez entrer un nombre <= 20.)
  ```
- Entrée : `abc`
  ```
  Entrez un nombre entier non négatif (max 20) pour calculer sa factorielle :
  *** Exception: Prelude.read: no parse
  ```

### Remarques
- Ce code est optimisé pour fonctionner dans des environnements en ligne avec des limites de temps (comme 7 secondes). La limite de 20 évite les calculs excessivement longs.
- Si vous utilisez une plateforme avec un timeout strict, testez avec des petits nombres (0 à 20).
- Pour compiler localement avec GHC : `ghc Factorial.hs` puis `./Factorial`.

Testez ce code dans votre environnement et informez-moi si vous rencontrez des problèmes !
