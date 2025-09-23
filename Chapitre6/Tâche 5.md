Voici un code Haskell qui implémente une fonction récursive pour inverser une liste, avec une fonction `main` incluse. La fonction utilise la récursion pour construire une nouvelle liste dans l'ordre inverse.

```haskell
-- Définition de la fonction pour inverser une liste
reverseList :: [a] -> [a]
reverseList [] = []           -- Cas de base : une liste vide reste vide
reverseList (x:xs) = reverseList xs ++ [x]  -- Cas récursif : ajoute x à la fin de l'inversion de xs

-- Fonction principale
main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5] :: [Integer]  -- Liste fixe pour tester
    print (reverseList myList)  -- Affiche la liste inversée
```

### Explications :
1. **Fonction `reverseList`** :
   - `reverseList :: [a] -> [a]` : Déclare que `reverseList` prend une liste de n'importe quel type (`[a]`) et retourne une liste du même type. L'utilisation de `a` rend la fonction polymorphe.
   - `reverseList [] = []` : Cas de base, une liste vide inversée reste vide.
   - `reverseList (x:xs) = reverseList xs ++ [x]` : Cas récursif, où :
     - `x` est le premier élément de la liste.
     - `xs` est le reste de la liste.
     - `reverseList xs` inverse récursivement le reste, et `++ [x]` ajoute `x` à la fin, inversant ainsi l'ordre. Par exemple, pour `[1, 2, 3]`, cela devient `reverseList [2, 3] ++ [1]`, puis `reverseList [3] ++ [2] ++ [1]`, et enfin `[] ++ [3] ++ [2] ++ [1] = [3, 2, 1]`.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let myList = [1, 2, 3, 4, 5] :: [Integer]` : Définit une liste fixe pour tester la fonction.
   - `print (reverseList myList)` : Affiche la liste inversée. Pour cette liste, le résultat est `[5, 4, 3, 2, 1]`.

### Remarques :
- Cette implémentation récursive utilise l'opérateur `++` pour concaténer, ce qui peut être inefficace pour de grandes listes car il parcourt la liste de gauche à droite à chaque étape. Une version plus performante utiliserait une accumulation avec une liste auxiliaire, mais celle-ci illustre bien la récursion.
- Pour tester avec une autre liste, change la valeur de `myList` (par exemple, `["a", "b", "c"]` pour obtenir `["c", "b", "a"]`).

Copie ce code dans ton éditeur et exécute-le. Si tu veux une version interactive ou des ajustements, fais-le-moi savoir !
