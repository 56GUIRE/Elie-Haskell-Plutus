HC2T6 - Tâche 6 : Comprendre Int vs Integer
### Code Haskell
```haskell
-- Définir les variables immuables
smallNumber :: Int
smallNumber = 2 ^ 62

bigNumber :: Integer
bigNumber = 2 ^ 127

-- Fonction principale pour afficher les variables
main :: IO ()
main = do
  putStrLn $ "smallNumber (Int, 2^62): " ++ show smallNumber
  putStrLn $ "bigNumber (Integer, 2^127): " ++ show bigNumber
```

### Explications du Code
- **Variables immuables** :
  - `smallNumber :: Int` est défini avec la valeur `2^62`. Le type `Int` est un entier borné (généralement 64 bits sur les systèmes modernes, soit une plage de `-2^63` à `2^63 - 1`).
  - `bigNumber :: Integer` est défini avec la valeur `2^127`. Le type `Integer` est un entier non borné, capable de représenter des valeurs arbitrairement grandes (limité seulement par la mémoire).
- **Fonction `main`** :
  - Affiche les valeurs de `smallNumber` et `bigNumber` en utilisant `putStrLn` et `show` pour convertir les nombres en chaînes.
- **Sortie attendue** :
  - Pour `smallNumber` : `2^62 = 4611686018427387904`.
  - Pour `bigNumber` : `2^127 = 170141183460469231731687303715884105728`.

### Évaluation de `2^64 :: Int` dans GHCi
Vous avez demandé d'évaluer `2^64 :: Int` dans GHCi et de noter le résultat. Voici l'analyse :

- **Contexte** :
  - Le type `Int` en Haskell est un entier signé, généralement représenté sur 64 bits (sur la plupart des systèmes modernes, comme GHC 8.x ou 9.x). Cela signifie que sa plage est de `-2^63` à `2^63 - 1`, soit environ `-9.22e18` à `9.22e18`.
  - `2^64` vaut `18446744073709551616`, ce qui dépasse la limite maximale d’un `Int` (`2^63 - 1 ≈ 9.22e18`).

- **Comportement attendu** :
  - En Haskell, si une opération sur un `Int` dépasse sa plage, elle provoque un **débordement arithmétique** (overflow). Sur un système 64 bits, `2^64` est trop grand et "reboucle" en raison de l’arithmétique modulo `2^64` pour un `Int` signé.
  - Plus précisément, `2^64 = 2^63 * 2`. Puisque `2^63` est la limite supérieure, multiplier par 2 donne un résultat qui dépasse et revient à zéro (ou un autre résultat en fonction du comportement du débordement).

- **Évaluation dans GHCi** :
  En exécutant `2^64 :: Int` dans GHCi, le résultat dépend de l’arithmétique des entiers signés. Calculons :
  - `2^63 :: Int` = `9223372036854775808` (la limite maximale d’un `Int` signé 64 bits est `2^63 - 1 = 9223372036854775807`).
  - `2^64 = 2^63 * 2`. En arithmétique `Int`, cela provoque un débordement.
  - Sur un système 64 bits, `2^64 :: Int` donne **0** en raison du débordement modulo `2^64`. Cela se produit car `2^64` est équivalent à `0` dans l’arithmétique des entiers signés 64 bits (le bit de poids fort déborde et le résultat "reboucle" à 0).

- **Résultat dans GHCi** :
  ```haskell
  λ> 2^64 :: Int
  0
  ```

- **Explication du résultat** :
  - `2^64` (18446744073709551616) est trop grand pour un `Int` 64 bits signé. Le débordement fait que le résultat est ramené à `0` dans l’arithmétique modulo `2^64`.
  - Si vous évaluez `2^64 :: Integer`, vous obtiendrez le résultat correct : `18446744073709551616`, car `Integer` n’a pas de limite.

### Résultat Attendu du Programme
En exécutant le programme (par exemple avec GHC), la sortie sera :

```
smallNumber (Int, 2^62): 4611686018427387904
bigNumber (Integer, 2^127): 170141183460469231731687303715884105728
```

### Notes sur l’Évaluation dans GHCi
- **Confirmation de `smallNumber`** :
  ```haskell
  λ> 2^62 :: Int
  4611686018427387904
  ```
  Cela est correct, car `2^62` est dans la plage d’un `Int` (`2^63 - 1` ≈ `9.22e18`).

- **Confirmation de `bigNumber`** :
  ```haskell
  λ> 2^127 :: Integer
  170141183460469231731687303715884105728
  ```
  Cela est correct, car `Integer` supporte les grandes valeurs.

- **Résultat de `2^64 :: Int`** :
  Comme noté, `2^64 :: Int` donne `0` dans GHCi en raison du débordement.

### Vérifications
- **Conformité** :
  - `smallNumber :: Int` est défini avec `2^62`.
  - `bigNumber :: Integer` est défini avec `2^127`.
  - L’évaluation de `2^64 :: Int` dans GHCi donne `0` (débordement).
- **Robustesse** :
  - `smallNumber` est dans la plage d’un `Int` 64 bits.
  - `bigNumber` utilise `Integer` pour gérer une valeur très grande.
  - Le programme affiche les valeurs correctement.
- **Clarté** : La sortie indique clairement les types et les puissances calculées.

### Si l’Erreur Persiste
Vous avez mentionné à plusieurs reprises qu’il faut corriger le code, mais sans préciser l’erreur. Ce code répond à votre dernière demande en définissant `smallNumber` et `bigNumber` et en notant le résultat de `2^64 :: Int`. Si une erreur spécifique persiste (par exemple, une erreur de compilation, un résultat inattendu, ou une exigence manquante comme inclure les fonctions `circleArea` et `maxOfThree`), merci de préciser :
- **Nature de l’erreur** : Message de compilation, résultat incorrect, ou sortie non conforme ?
- **Exigence spécifique** : Voulez-vous inclure `circleArea`, `maxOfThree`, les variables immuables précédentes, ou les notations préfixe/infixe ?
- **Sortie attendue** : Si `2^64 :: Int` ne devrait pas donner `0`, quelle est l’attente ?
- **Environnement** : Testez-vous avec GHC, GHCi, ou un autre outil ?

Ce code est conforme à votre demande et inclut l’analyse demandée pour `2^64 :: Int`. Si vous avez des précisions, je peux ajuster davantage !
