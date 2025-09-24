Voici le code Haskell pour créer une fonction `extract` qui prend une valeur par défaut et une `Box a`, et renvoie la valeur contenue dans la boîte ou la valeur par défaut si la boîte est vide. Le type `Box` est défini comme précédemment, et un `main` est inclus pour tester la fonction.

```haskell
-- Définition du type Box
data Box a = Empty | Value a deriving (Show)

-- Fonction extract qui renvoie la valeur dans la Box ou la valeur par défaut
extract :: a -> Box a -> a
extract defaultValue Empty = defaultValue      -- Si la boîte est vide, renvoie la valeur par défaut
extract _ (Value x) = x                       -- Si la boîte contient une valeur, renvoie cette valeur

-- Fonction main pour tester
main :: IO ()
main = do
    let box1 = Value 42       -- Une boîte contenant 42
    let box2 = Empty          -- Une boîte vide
    print $ extract 0 box1    -- Devrait afficher 42
    print $ extract 0 box2    -- Devrait afficher 0
```

### Explications :
1. **Type `Box`** : Le type `Box a` est un type algébrique avec deux constructeurs : `Empty` (boîte vide) et `Value a` (boîte contenant une valeur de type `a`).
2. **Fonction `extract`** :
   - Prend une valeur par défaut (`defaultValue`) et une `Box a`.
   - Si la boîte est `Empty`, renvoie `defaultValue`.
   - Si la boîte contient une valeur (`Value x`), renvoie `x`.
3. **Main** : Teste la fonction avec une boîte contenant 42 (renvoie 42) et une boîte vide (renvoie la valeur par défaut, 0).

### Résultat attendu :
En exécutant le programme, la sortie sera :
```
42
0
```

Ce code est simple, idiomatique en Haskell, et répond à la demande. Si tu as besoin d'autres fonctionnalités ou d'explications, n'hésite pas !
