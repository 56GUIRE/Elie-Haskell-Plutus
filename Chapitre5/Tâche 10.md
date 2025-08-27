HC5T10 : Combiner les fonctions d'ordre supérieur


```haskell
main :: IO ()
main = do
    let numbers = [4, 5, 8, 2, 10]
    print $ hasSquareAbove50 numbers  -- Affiche True

hasSquareAbove50 :: [Int] -> Bool
hasSquareAbove50 xs = any (>50) $ map (^2) xs
```

### Explications :
- **Fonction `hasSquareAbove50`** :
  - **Type** : Prend une liste d'entiers (`[Int]`) et retourne un booléen (`Bool`).
  - **Opération** :
    - `map (^2) xs` applique l'opération de mise au carré (`^2`) à chaque élément de la liste `xs`.
    - `any (>50)` vérifie si au moins un élément du résultat est supérieur à 50.
    - L'opérateur `$` est utilisé pour éviter les parenthèses, rendant l'expression plus lisible : `any (>50) $ map (^2) xs` équivaut à `any (>50) (map (^2) xs)`.
  - **Exemple** : Pour `xs = [4, 5, 8, 2, 10]` :
    - `map (^2) xs` donne `[16, 25, 64, 4, 100]`.
    - `any (>50)` vérifie si un élément est `> 50` : `64` et `100` le sont, donc la fonction retourne `True`.
- **Main** : Teste la fonction avec la liste `[4, 5, 8, 2, 10]` et affiche le résultat (`True`).

### Résultat :
Pour la liste `[4, 5, 8, 2, 10]`, le programme affiche `True` car `8^2 = 64` et `10^2 = 100` sont supérieurs à 50.

Ce code est concis, combine `filter`, `map`, et `any` comme demandé (bien que `filter` ne soit pas explicitement nécessaire ici, `any` suffit pour la vérification), et inclut un `main`. Si vous voulez une version utilisant explicitement `filter` (par exemple, pour filtrer les carrés avant de vérifier), voici une alternative :

```haskell
main :: IO ()
main = do
    let numbers = [4, 5, 8, 2, 10]
    print $ hasSquareAbove50 numbers  -- Affiche True

hasSquareAbove50 :: [Int] -> Bool
hasSquareAbove50 xs = not $ null $ filter (>50) $ map (^2) xs
```

Dans cette version, `filter (>50)` est utilisé pour ne garder que les carrés supérieurs à 50, et `not $ null` vérifie si la liste résultante n'est pas vide. Le résultat est identique.

