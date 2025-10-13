Je vais vous fournir un programme Haskell qui définit un type personnalisé `Result a`, utilise le pattern matching avec le symbole `@` (as-pattern), et inclut un `main` pour démontrer son fonctionnement.

### Code Haskell (fichier `Main.hs`)

```haskell
module Main where

-- Définition du type personnalisé Result
data Result a = Success a | Failure String deriving (Show)

-- Fonction utilisant le pattern matching avec @
processResult :: Result Int -> String
processResult result@(Success n) = "Succès avec la valeur " ++ show n ++ " (structure complète : " ++ show result ++ ")"
processResult result@(Failure msg) = "Échec avec le message : " ++ msg ++ " (structure complète : " ++ show result ++ ")"

main :: IO ()
main = do
    let successCase = Success 42
        failureCase = Failure "Une erreur s'est produite"
    
    putStrLn $ processResult successCase
    putStrLn $ processResult failureCase
```

### Explications
- **Type personnalisé `Result a` :**
  - Déclaré avec `data Result a = Success a | Failure String`.
  - `Result` est un type paramétrique qui peut contenir une valeur de type `a` dans le cas `Success`, ou un message d'erreur de type `String` dans le cas `Failure`.
  - L'option `deriving (Show)` permet d'afficher les valeurs de `Result` avec `show`.

- **Pattern matching avec `@` (as-pattern) :**
  - Dans la fonction `processResult`, le symbole `@` est utilisé pour lier la structure complète du `Result` à la variable `result`, tout en décomposant ses parties.
  - Pour `result@(Success n)`, `result` contient toute la structure (par exemple, `Success 42`), et `n` contient la valeur `42`.
  - Pour `result@(Failure msg)`, `result` contient toute la structure (par exemple, `Failure "Une erreur..."`), et `msg` contient le message d'erreur.
  - Cela permet d'accéder à la fois à la structure entière et à ses composants dans le pattern matching.

- **Programme principal (`main`) :**
  - Crée deux exemples : `Success 42` et `Failure "Une erreur s'est produite"`.
  - Appelle `processResult` pour chaque cas et affiche les résultats.

### Sortie attendue
Lorsque vous exécutez ce programme, la sortie sera :
```
Succès avec la valeur 42 (structure complète : Success 42)
Échec avec le message : Une erreur s'est produite (structure complète : Failure "Une erreur s'est produite")
```

### Instructions pour exécuter le programme

1. **Structure du projet :**
   Créez un dossier avec un fichier `Main.hs` contenant le code ci-dessus. Si vous utilisez Cabal, voici un exemple de fichier `.cabal` minimal :

   ```cabal
   cabal-version:       3.0
   name:                result-pattern
   version:             0.1.0.0
   synopsis:            A program demonstrating pattern matching with Result type
   license:             BSD-3-Clause
   author:              Votre Nom
   maintainer:          votre.email@example.com
   build-type:          Simple

   executable result-pattern
     main-is:             Main.hs
     build-depends:       base ^>= 4.16
     hs-source-dirs:      src
     default-language:    Haskell2010
   ```

2. **Exécution avec Cabal :**
   - Placez `Main.hs` dans un dossier `src`.
   - Dans le dossier du projet, exécutez :
     ```bash
     cabal update
     cabal build
     cabal run result-pattern
     ```

3. **Exécution avec Stack :**
   - Créez un projet avec `stack new result-pattern`.
   - Le fichier `package.yaml` n'a pas besoin de dépendances supplémentaires, car seules `base` est utilisée.
   - Exécutez :
     ```bash
     stack build
     stack exec result-pattern
     ```

4. **Exécution directe avec GHC :**
   - Compilez et exécutez directement :
     ```bash
     ghc Main.hs
     ./Main
     ```

### Remarques
- **Utilité de `@` :** Le pattern matching avec `@` est particulièrement utile lorsque vous avez besoin à la fois de la structure complète et de ses composants. Par exemple, ici, il permet d'afficher à la fois la valeur extraite (`n` ou `msg`) et la structure entière (`result`).
- **Type paramétrique :** `Result a` est générique, donc vous pouvez remplacer `Int` par un autre type (par exemple, `Result String` ou `Result Double`) sans modifier la logique de `processResult`.
- **Extensions :** Ce programme n'utilise aucune extension spécifique de GHC, donc il est compatible avec toutes les versions récentes de GHC.

Si vous souhaitez ajouter des fonctionnalités (par exemple, lire des entrées utilisateur, ajouter plus de cas pour `Result`, ou utiliser d'autres extensions), faites-le-moi savoir !
