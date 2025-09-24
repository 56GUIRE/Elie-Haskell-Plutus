Voici le code Haskell pour créer une fonction `addN` qui prend un nombre et une `Box` contenant potentiellement un nombre, et ajoute ce nombre à la valeur contenue dans la `Box`. Je vais d'abord définir un type `Box` (comme une boîte pouvant être vide ou contenir une valeur), puis implémenter la fonction `addN` avec un `main` pour tester.

```haskell
-- Définition du type Box
data Box a = Empty | Value a deriving (Show)

-- Fonction addN qui ajoute un nombre à la valeur dans la Box
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty           -- Si la boîte est vide, retourne Empty
addN n (Value x) = Value (x + n) -- Si la boîte contient une valeur, ajoute n

-- Fonction main pour tester
main :: IO ()
main = do
    let box1 = Value 10        -- Une boîte contenant 10
    let box2 = Empty           -- Une boîte vide
    print $ addN 5 box1        -- Devrait afficher Value 15
    print $ addN 5 box2        -- Devrait afficher Empty
```

### Explications :
1. **Type `Box`** : Le type `Box a` est défini comme un type algébrique avec deux constructeurs : `Empty` (boîte vide) et `Value a` (boîte contenant une valeur de type `a`).
2. **Fonction `addN`** : 
   - Prend un nombre `n` et une `Box a` où `a` est de la classe `Num` (pour permettre l'addition).
   - Si la boîte est `Empty`, retourne `Empty`.
   - Si la boîte contient une valeur (`Value x`), retourne une nouvelle boîte avec la valeur `x + n`.
3. **Main** : Teste la fonction avec une boîte contenant 10 (ajoute 5 pour obtenir 15) et une boîte vide (reste vide).

### Résultat attendu :
En exécutant le programme, la sortie sera :
```
Value 15
Empty
```

Ce code est concis, fonctionnel et respecte les conventions de Haskell. Si tu as besoin d'explications supplémentaires ou de modifications, fais-le-moi savoir !
