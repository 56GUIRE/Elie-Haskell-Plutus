Je vais vous fournir un programme Haskell qui utilise l'extension `TypeApplications` pour créer une fonction convertissant une `String` en `Int` avec `read`, et inclut un `main` pour démontrer son utilisation.

### Code Haskell (fichier `Main.hs`)

```haskell
{-# LANGUAGE TypeApplications #-}

module Main where

-- Fonction pour convertir une String en Int
stringToInt :: String -> Int
stringToInt s = read @Int s

main :: IO ()
main = do
    let input = "42"  -- Exemple de chaîne
    let result = stringToInt input
    putStrLn $ "Conversion de \"" ++ input ++ "\" en Int : " ++ show result
```

### Explications
- **Extension `TypeApplications` :**
  - Activée avec la pragma `{-# LANGUAGE TypeApplications #-}`.
  - Permet de spécifier explicitement le type attendu pour une fonction polymorphe comme `read` en utilisant la syntaxe `@Type`.
  - Ici, `read @Int s` indique que `read` doit convertir la chaîne `s` en un `Int`.

- **Fonction `stringToInt` :**
  - Prend une `String` en entrée et utilise `read @Int` pour la convertir en `Int`.
  - La syntaxe `@Int` clarifie que le résultat de `read` doit être un `Int`, évitant l'ambiguïté de type.

- **Programme principal (`main`) :**
  - Définit une chaîne d'exemple (`"42"`).
  - Appelle `stringToInt` pour convertir la chaîne en `Int`.
  - Affiche le résultat avec un message descriptif.

### Sortie attendue
Lorsque vous exécutez ce programme, la sortie sera :
```
Conversion de "42" en Int : 42
```

### Instructions pour exécuter le programme

1. **Structure du projet :**
   Créez un dossier avec un fichier `Main.hs` contenant le code ci-dessus. Si vous utilisez Cabal, voici un exemple de fichier `.cabal` minimal :

   ```cabal
   cabal-version:       3.0
   name:                string-to-int
   version:             0.1.0.0
   synopsis:            A program to convert a String to Int
   license:             BSD-3-Clause
   author:              Votre Nom
   maintainer:          votre.email@example.com
   build-type:          Simple

   executable string-to-int
     main-is:             Main.hs
     build-depends:       base ^>= 4.16
     hs-source-dirs:      src
     default-language:    Haskell2010
     ghc-options:         -XTypeApplications
   ```

   - `ghc-options: -XTypeApplications` active l'extension au niveau du compilateur, bien que la pragma dans `Main.hs` soit généralement suffisante.

2. **Exécution avec Cabal :**
   - Placez `Main.hs` dans un dossier `src`.
   - Dans le dossier du projet, exécutez :
     ```bash
     cabal update
     cabal build
     cabal run string-to-int
     ```

3. **Exécution avec Stack :**
   - Si vous utilisez Stack, créez un projet avec `stack new string-to-int`.
   - Ajoutez `TypeApplications` dans le fichier `package.yaml` sous `extensions` :
     ```yaml
     extensions:
       - TypeApplications
     ```
   - Exécutez :
     ```bash
     stack build
     stack exec string-to-int
     ```

4. **Exécution directe avec GHC :**
   - Si vous n'utilisez pas Cabal ou Stack, compilez et exécutez directement :
     ```bash
     ghc -XTypeApplications Main.hs
     ./Main
     ```

### Remarques
- **Gestion des erreurs :** Le programme suppose que la chaîne d'entrée est valide (par exemple, `"42"`). Si la chaîne ne représente pas un entier valide (par exemple, `"abc"`), `read` lèvera une exception. Pour une gestion robuste, vous pouvez utiliser `readMaybe` du module `Text.Read` :
  ```haskell
  import Text.Read (readMaybe)

  stringToInt :: String -> Maybe Int
  stringToInt s = readMaybe @Int s
  ```
  Cela renverrait `Just 42` pour `"42"` et `Nothing` pour une entrée invalide.

- **TypeApplications :** Cette extension est utile pour clarifier les types dans des contextes où une fonction polymorphe comme `read` pourrait être ambiguë. Elle est disponible à partir de GHC 8.0.1.

- **Personnalisation :** Vous pouvez modifier `main` pour lire une entrée utilisateur avec `getLine` au lieu d'utiliser une constante :
  ```haskell
  main :: IO ()
  main = do
      putStrLn "Entrez un nombre : "
      input <- getLine
      let result = stringToInt input
      putStrLn $ "Conversion de \"" ++ input ++ "\" en Int : " ++ show result
  ```

Si vous souhaitez ajouter une gestion d'erreurs, lire depuis l'entrée utilisateur, ou d'autres fonctionnalités, faites-le-moi savoir !
