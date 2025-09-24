Voici une solution en Haskell qui répond à votre demande. Nous allons créer des synonymes de type `Address` et `Value`, puis définir une fonction `generateTx` qui concatène deux adresses et une valeur dans une chaîne. Le programme inclut également une fonction `main` pour tester le résultat.

### Code Haskell :
```haskell
-- Définir les synonymes de type
type Address = String
type Value = Int

-- Fonction generateTx
generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr value = fromAddr ++ " -> " ++ toAddr ++ ": " ++ show value

-- Fonction main pour tester
main :: IO ()
main = do
  let from = "Addr1" :: Address
      to = "Addr2" :: Address
      val = 100 :: Value
  putStrLn $ generateTx from to val
```

### Explications :
1. **Synonymes de type** :
   - `type Address = String` crée un synonyme de type où `Address` représente un `String`. Cela améliore la lisibilité et la clarté du code.
   - `type Value = Int` crée un synonyme de type où `Value` représente un `Int`.

2. **Fonction `generateTx`** :
   - La fonction prend trois paramètres : `fromAddr` (type `Address`), `toAddr` (type `Address`), et `value` (type `Value`).
   - Elle concatène les deux adresses et la valeur en utilisant l'opérateur `++` pour joindre les chaînes. La valeur est convertie en chaîne avec `show` pour pouvoir être concaténée.
   - Le format de sortie est `fromAddr -> toAddr: value` (par exemple, "Addr1 -> Addr2: 100").

3. **Fonction `main`** :
   - Définit des valeurs de test : `from = "Addr1"`, `to = "Addr2"`, et `val = 100`.
   - Utilise `putStrLn` pour afficher le résultat de `generateTx from to val`.

### Résultat attendu :
Lorsque vous exécutez ce programme, il affichera :
```
Addr1 -> Addr2: 100
```

Ce code est simple, fonctionnel et respecte les exigences. Vous pouvez modifier les valeurs dans `main` ou ajuster le format de la chaîne dans `generateTx` si nécessaire !
