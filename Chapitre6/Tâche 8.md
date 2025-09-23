HC6T8 : Filtrer les nombres paires

```haskell
-- Définition de la fonction pour filtrer les nombres pairs
filterEvens :: [Integer] -> [Integer]
filterEvens [] = []           -- Cas de base : une liste vide retourne une liste vide
filterEvens (x:xs)            -- Cas récursif : vérifie si x est pair
    | even x = x : filterEvens xs  -- Si x est pair, l'ajoute au résultat et continue
    | otherwise = filterEvens xs   -- Sinon, ignore x et continue avec le reste

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5, 6] :: [Integer]  -- Liste fixe pour tester
    print (filterEvens myList)  -- Affiche la liste filtrée (nombres pairs)
```

### Explications :
1. **Fonction `filterEvens`** :
   - `filterEvens :: [Integer] -> [Integer]` : Déclare que `filterEvens` prend une liste d'entiers (`[Integer]`) comme entrée et retourne une liste d'entiers.
   - `filterEvens [] = []` : Cas de base, une liste vide retourne une liste vide.
   - `filterEvens (x:xs)` : Cas récursif, où :
     - `x` est le premier élément de la liste.
     - `xs` est le reste de la liste.
     - `even x` vérifie si `x` est pair (utilise la fonction `even` de la Prelude).
     - Si oui, ajoute `x` au résultat suivi du filtrage du reste (`x : filterEvens xs`).
     - Sinon, continue sans `x` (`filterEvens xs`).
     - Par exemple, pour `[1, 2, 3, 4]`, cela retourne `[2, 4]`.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5, 6] :: [Integer]` : Définit une liste fixe pour tester la fonction.
   - `print (filterEvens myList)` : Affiche la liste filtrée. Pour cette liste, le résultat est `[2,4,6]`.
