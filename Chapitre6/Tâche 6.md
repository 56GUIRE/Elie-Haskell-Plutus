Voici un code Haskell qui implémente une fonction récursive pour déterminer si un élément donné existe dans une liste, avec une fonction `main` incluse. La fonction utilise la récursion pour parcourir la liste et vérifier la présence de l'élément.

```haskell
-- Définition de la fonction pour vérifier si un élément existe dans une liste
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False    -- Cas de base : une liste vide ne contient pas l'élément
elementExists x (y:ys)        -- Cas récursif : vérifie l'élément courant et le reste
    | x == y = True          -- Si l'élément courant est égal à x, retourne True
    | otherwise = elementExists x ys  -- Sinon, continue avec le reste de la liste

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5] :: [Integer]  -- Liste fixe pour tester
    let elementToFind = 3 :: Integer           -- Élément à rechercher
    print (elementExists elementToFind myList)  -- Affiche True si l'élément existe
```

### Explications :
1. **Fonction `elementExists`** :
   - `elementExists :: Eq a => a -> [a] -> Bool` : Déclare que `elementExists` prend un élément de type `a` et une liste de ce type (`[a]`), et retourne un booléen (`Bool`). La contrainte `Eq a` indique que le type `a` doit supporter l'égalité (`==`), nécessaire pour comparer les éléments.
   - `elementExists _ [] = False` : Cas de base, si la liste est vide, l'élément n'existe pas (retourne `False`).
   - `elementExists x (y:ys)` : Cas récursif, où :
     - `x` est l'élément recherché.
     - `y` est le premier élément de la liste.
     - `ys` est le reste de la liste.
     - `x == y = True` : Si l'élément courant `y` est égal à `x`, retourne `True`.
     - `otherwise = elementExists x ys` : Sinon, continue la recherche dans le reste de la liste.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5] :: [Integer]` : Définit une liste fixe pour tester la fonction.
   - `let elementToFind = 3 :: Integer` : Définit l'élément à rechercher.
   - `print (elementExists elementToFind myList)` : Affiche le résultat. Pour cet exemple, cela retourne `True` car 3 est dans la liste.

### Remarques :
- Cette implémentation est simple et récursive. Elle parcourt la liste jusqu'à trouver l'élément ou atteindre la fin.
- Pour tester avec un autre élément ou liste, change `elementToFind` ou `myList` (par exemple, `elementToFind = 6` donnera `False`).
- Une version plus efficace pour de grandes listes pourrait utiliser une recherche binaire si la liste était triée, mais cela nécessite une modification supplémentaire.

Copie ce code dans ton éditeur et exécute-le. Si tu veux une version interactive ou des ajustements, fais-le-moi savoir !
