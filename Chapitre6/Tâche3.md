

```haskell
-- Définition de la fonction de somme utilisant foldr
sumList :: [Integer] -> Integer
sumList xs = foldr (+) 0 xs  -- foldr applique l'addition (+) à droite, avec 0 comme accumulateur initial

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5] :: [Integer]  -- Liste fixe pour tester
    print (sumList myList)  -- Affiche la somme de la liste
```

### Explications :
1. **Fonction `sumList`** :
   - `sumList :: [Integer] -> Integer` : Déclare que `sumList` prend une liste d'entiers (`[Integer]`) comme entrée et retourne un entier.
   - `foldr (+) 0 xs` : Utilise `foldr` pour sommer les éléments. 
     - `(+)` est l'opérateur binaire pour l'addition.
     - `0` est la valeur initiale (accumulateur de départ).
     - `xs` est la liste à sommer.
     - Par exemple, pour `[1, 2, 3]`, cela équivaut à `1 + (2 + (3 + 0))`.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5] :: [Integer]` : Définit une liste fixe pour tester la fonction sans entrée interactive.
   - `print (sumList myList)` : Calcule et affiche la somme. Pour cette liste, le résultat est 15 (1 + 2 + 3 + 4 + 5).
