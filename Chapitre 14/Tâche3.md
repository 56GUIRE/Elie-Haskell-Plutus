Je vais vous fournir un programme Haskell qui utilise l'extension `NumericUnderscores` pour définir et afficher des variables contenant de grands nombres. Cette extension permet d'utiliser des underscores (`_`) pour améliorer la lisibilité des grands nombres dans le code.

### Code Haskell (fichier `Main.hs`)

```haskell
{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = do
    let bigNumber1 = 1_000_000_000 :: Integer -- 1 milliard
        bigNumber2 = 123_456_789_012 :: Integer -- 123 milliards et plus
        bigNumber3 = 9_999_999_999_999_999 :: Integer -- Presque 10 quadrillions

    putStrLn $ "Grand nombre 1 : " ++ show bigNumber1
    putStrLn $ "Grand nombre 2 : " ++ show bigNumber2
    putStrLn $ "Grand nombre 3 : " ++ show bigNumber3
```

### Explications
- **Extension `NumericUnderscores` :**
  - Activée avec la pragma `{-# LANGUAGE NumericUnderscores #-}` en haut du fichier.
  - Permet d'écrire des nombres avec des underscores pour séparer les groupes de chiffres (par exemple, `1_000_000_000` pour un milliard).
  - Les underscores sont ignorés par le compilateur et servent uniquement à améliorer la lisibilité.

- **Variables :**
  - `bigNumber1`, `bigNumber2`, et `bigNumber3` sont définies avec le type `Integer` pour gérer des nombres arbitrairement grands (pas de limite de taille contrairement à `Int`).
  - Les nombres utilisent des underscores pour une meilleure clarté.

- **Affichage :**
  - `putStrLn` affiche chaque nombre avec un message descriptif.
  - La fonction `show` convertit les nombres en chaînes pour l'affichage.

### Sortie attendue
Lorsque vous exécutez ce programme, la sortie sera :
```
Grand nombre 1 : 1000000000
Grand nombre 2 : 123456789012
Grand nombre 3 : 9999999999999999
```

### Instructions pour exécuter le programme

1. **Structure du projet :**
   Créez un dossier avec un fichier `Main.hs` contenant le code ci-dessus. Si vous utilisez Cabal ou Stack, voici un exemple de fichier `.cabal` minimal :

   ```cabal
   cabal-version:       3.0
   name:                big-numbers
   version:             0.1.0.0
   synopsis:            A program to display large numbers
   license:             BSD-3-Clause
   author:              Votre Nom
   maintainer:          votre.email@example.com
   build-type:          Simple

   executable big-numbers
     main-is:             Main.hs
     build-depends:       base ^>= 4.16
     hs-source-dirs:      src
     default-language:    Haskell2010
     ghc-options:         -XNumericUnderscores
   ```

   - Notez que `ghc-options: -XNumericUnderscores` active l'extension au niveau du compilateur, bien que la pragma dans le fichier `Main.hs` soit généralement suffisante.

2. **Exécution avec Cabal :**
   - Placez `Main.hs` dans un dossier `src`.
   - Dans le dossier du projet, exécutez :
     ```bash
     cabal update
     cabal build
     cabal run big-numbers
     ```

3. **Exécution avec Stack :**
   - Si vous utilisez Stack, créez un projet avec `stack new big-numbers`.
   - Ajoutez `NumericUnderscores` dans le fichier `package.yaml` sous `extensions` :
     ```yaml
     extensions:
       - NumericUnderscores
     ```
   - Exécutez :
     ```bash
     stack build
     stack exec big-numbers
     ```

4. **Exécution directe avec GHC :**
   - Si vous n'utilisez pas Cabal ou Stack, vous pouvez compiler et exécuter directement :
     ```bash
     ghc -XNumericUnderscores Main.hs
     ./Main
     ```

### Remarques
- L'extension `NumericUnderscores` est disponible à partir de GHC 8.6.1. Assurez-vous que votre version de GHC est compatible (`ghc --version`).
- Les nombres dans le programme utilisent `Integer` pour éviter les limitations de taille. Si vous utilisez `Int`, les nombres trop grands pourraient provoquer un débordement.
- Vous pouvez modifier les valeurs des nombres ou ajouter d'autres opérations (comme des calculs) si besoin.

Si vous avez d'autres exigences (par exemple, formater l'affichage ou effectuer des calculs avec ces nombres), faites-le-moi savoir !
