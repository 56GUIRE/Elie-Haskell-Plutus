HC14T3 : Extension de traits de soulignement numériques
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

