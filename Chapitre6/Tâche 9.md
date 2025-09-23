Voici un code Haskell qui implémente une fonction récursive similaire à `map`, qui applique une fonction donnée à chaque élément d'une liste et retourne une nouvelle liste avec les résultats, avec une fonction `main` incluse.

```haskell
-- Définition de la fonction myMap (similaire à map)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []               -- Cas de base : une liste vide retourne une liste vide
myMap f (x:xs) = f x : myMap f xs  -- Cas récursif : applique f à x et continue avec le reste

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5] :: [Integer]  -- Liste fixe pour tester
    let double x = x * 2                       -- Fonction exemple à appliquer (double chaque élément)
    print (myMap double myList)                -- Affiche la liste résultante
```

### Explications :
1. **Fonction `myMap`** :
   - `myMap :: (a -> b) -> [a] -> [b]` : Déclare que `myMap` prend une fonction `(a -> b)` (qui transforme un élément de type `a` en `b`) et une liste de type `[a]`, et retourne une liste de type `[b]`. C'est polymorphe grâce à `a` et `b`.
   - `myMap _ [] = []` : Cas de base, une liste vide retourne une liste vide.
   - `myMap f (x:xs) = f x : myMap f xs` : Cas récursif, où :
     - `f` est la fonction à appliquer.
     - `x` est le premier élément de la liste.
     - `xs` est le reste de la liste.
     - Applique `f` à `x` et l'ajoute au début du résultat du `myMap` sur le reste. Par exemple, pour `double` sur `[1, 2, 3]`, cela donne `[2, 4, 6]`.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5] :: [Integer]` : Définit une liste fixe pour tester.
   - `let double x = x * 2` : Définit une fonction exemple qui double un nombre.
   - `print (myMap double myList)` : Applique `myMap` et affiche le résultat. Pour cette liste, cela donne `[2,4,6,8,10]`.

### Remarques :
- Cette implémentation est récursive et équivalente à la fonction `map` de la bibliothèque standard de Haskell. Elle est efficace pour des listes de taille raisonnable.
- Pour tester avec une autre fonction ou liste, change `double` ou `myList` (par exemple, `let square x = x * x` pour obtenir les carrés : `[1,4,9,16,25]`).

Copie ce code dans ton éditeur et exécute-le. Si tu veux une version interactive ou des ajustements, fais-le-moi savoir !
