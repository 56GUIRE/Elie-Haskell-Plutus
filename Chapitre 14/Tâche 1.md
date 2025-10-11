Le code Haskell pour le programme qui affiche « Hello, Cabal ! » se trouve dans le fichier `app/Main.hs`. Voici le code exact :

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Cabal!"
```

### Explications :
- Ce code est placé dans le dossier `app` de ton projet Haskell, créé avec `cabal init`.
- `module Main where` : Déclare le module principal, requis pour un exécutable.
- `main :: IO ()` : La fonction `main` est le point d'entrée du programme.
- `putStrLn "Hello, Cabal!"` : Affiche le texte « Hello, Cabal ! » dans la console.

### Contexte du projet :
Pour que ce code fonctionne, ton projet doit avoir un fichier `hello-cabal.cabal` (généré par `cabal init`) qui configure l'exécutable. Voici un exemple minimal de ce fichier :

```cabal
cabal-version:      2.4
name:               hello-cabal
version:            0.1.0.0

executable hello-cabal
    main-is:          Main.hs
    build-depends:    base ^>= 4.14
    hs-source-dirs:   app
    default-language: Haskell2010
```

### Comment exécuter :
1. Place le code Haskell ci-dessus dans `app/Main.hs`.
2. Assure-toi que le fichier `.cabal` est correctement configuré.
3. Dans le terminal, à la racine du projet, exécute :
   ```bash
   cabal build
   cabal run hello-cabal
   ```
4. Tu verras s'afficher :
   ```
   Hello, Cabal!
   ```

Si tu faisais référence à un autre aspect du code ou si tu as une question spécifique, précise-le et je t'aiderai !
