Voici un code Haskell qui implémente une fonction récursive pour calculer la longueur d'une liste, avec une fonction `main` incluse. La fonction utilise la récursion pour compter les éléments de la liste.

```haskell
-- Définition de la fonction pour calculer la longueur d'une liste
lengthList :: [a] -> Integer
lengthList [] = 0              -- Cas de base : une liste vide a une longueur de 0
lengthList (x:xs) = 1 + lengthList xs  -- Cas récursif : ajoute 1 au compte du reste

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5] :: [Integer]  -- Liste fixe pour tester
    print (lengthList myList)  -- Affiche la longueur de la liste
```

### Explications :
1. **Fonction `lengthList`** :
   - `lengthList :: [a] -> Integer` : Déclare que `lengthList` prend une liste de n'importe quel type (`[a]`) comme entrée et retourne un entier (`Integer`). L'utilisation de `a` rend la fonction polymorphe.
   - `lengthList [] = 0` : Cas de base, la longueur d'une liste vide est 0.
   - `lengthList (x:xs) = 1 + lengthList xs` : Cas récursif, où :
     - `x` est le premier élément de la liste.
     - `xs` est le reste de la liste.
     - Ajoute 1 (pour l'élément courant) à la longueur du reste de la liste. Par exemple, pour `[1, 2, 3]`, cela devient `1 + lengthList [2, 3]`, puis `1 + (1 + lengthList [3])`, et enfin `1 + (1 + (1 + lengthList [])) = 3`.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5] :: [Integer]` : Définit une liste fixe pour tester la fonction.
   - `print (lengthList myList)` : Affiche la longueur de la liste. Pour cette liste, le résultat est 5.

### Remarques :
- Cette implémentation récursive est simple mais peut être moins efficace que la fonction `length` de la bibliothèque standard pour de très grandes listes, car elle parcourt la liste entièrement à chaque appel. Cependant, elle illustre bien le principe de la récursion.
- Pour tester avec une autre liste, change la valeur de `myList` (par exemple, `["a", "b"]` pour obtenir 2).

Copie ce code dans ton éditeur et exécute-le. Si tu veux une version interactive ou des ajustements, fais-le-moi savoir !
