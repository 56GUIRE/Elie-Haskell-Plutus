Voici un code Haskell qui implémente une fonction pour calculer la somme des éléments d'une liste en utilisant `foldr`, avec une fonction `main` pour interagir avec l'utilisateur. La fonction `foldr` applique une opération associative (ici, l'addition) de droite à gauche sur les éléments de la liste.

```haskell
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
```

### Explications
1. **Fonction `sumList`** :
   - Utilise `foldr (+) 0 xs` où :
     - `+` est l'opérateur d'addition.
     - `0` est l'élément neutre (identité pour l'addition).
     - `xs` est la liste d'entiers.
   - `foldr` applique l'addition de droite à gauche, par exemple pour `[1, 2, 3]`, cela équivaut à `1 + (2 + (3 + 0))`.

2. **Fonction `main`** :
   - Demande une entrée utilisateur sous forme de chaîne (par exemple, "1 2 3 4").
   - Utilise `words` pour diviser la chaîne en une liste de sous-chaînes séparées par des espaces.
   - Applique `map read` pour convertir chaque sous-chaîne en `Integer`.
   - Calcule la somme avec `sumList` et affiche le résultat.
   - Utilise `catch` pour gérer les erreurs (par exemple, si l'entrée contient des lettres ou est mal formatée).

### Exemples d'exécution
- Entrée : `1 2 3 4`
  ```
  Entrez une liste d'entiers séparés par des espaces (ex: 1 2 3 4) :
  La somme des éléments est : 10
  ```

- Entrée : `1 a 3`
  ```
  Entrez une liste d'entiers séparés par des espaces (ex: 1 2 3 4) :
  Erreur : Prelude.read: no parse
  ```

- Entrée : `` (vide)
  ```
  Entrez une liste d'entiers séparés par des espaces (ex: 1 2 3 4) :
  La somme des éléments est : 0
  ```

### Remarques
- **Type `Integer`** : Utilisé pour éviter les débordements avec de grandes sommes. Si vous voulez des nombres plus petits, vous pouvez remplacer `Integer` par `Int`.
- **Gestion des erreurs** : L'erreur est capturée mais peut être vague (par exemple, "no parse"). Pour une meilleure gestion, une validation explicite pourrait être ajoutée.
- **Compilation locale** : Sauvegardez dans `SumList.hs`, compilez avec `ghc SumList.hs`, puis exécutez avec `./SumList`.
- **Environnement en ligne** : Testez dans votre éditeur (comme CompilerBerry) en cliquant sur "RUN".

Testez ce code et informez-moi si vous avez des questions ou des ajustements à faire !
