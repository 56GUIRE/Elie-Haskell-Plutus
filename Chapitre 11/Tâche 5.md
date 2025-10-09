Je vais vous fournir un programme Haskell qui implémente une fonction `guessWhatsInside` pour vérifier si un élément spécifique se trouve dans un `Container`, avec un `main` pour tester la fonction.

### Hypothèses :
- Un `Container` peut être représenté comme une liste (`[a]`) pour simplifier, car c'est une structure courante en Haskell pour contenir des éléments.
- La fonction `guessWhatsInside` prend un `Container` (une liste) et un élément, et retourne un `Bool` indiquant si l'élément est présent.
- L'élément doit être comparable, donc on utilisera la contrainte de type `Eq` pour permettre la comparaison.

### Code Haskell :

```haskell
-- Définition d'un type Container (ici, une liste)
type Container a = [a]

-- Fonction guessWhatsInside qui vérifie si un élément est dans le container
guessWhatsInside :: (Eq a) => a -> Container a -> Bool
guessWhatsInside item container = item `elem` container

-- Fonction main pour tester
main :: IO ()
main = do
    let myContainer = [1, 2, 3, 4, 5] :: Container Int
    let item1 = 3
    let item2 = 6
    
    putStrLn $ "Container: " ++ show myContainer
    putStrLn $ "Est-ce que " ++ show item1 ++ " est dans le container ? " ++ show (guessWhatsInside item1 myContainer)
    putStrLn $ "Est-ce que " ++ show item2 ++ " est dans le container ? " ++ show (guessWhatsInside item2 myContainer)
```

### Explications :
1. **Type `Container`** : J'ai défini `Container a` comme une liste `[a]` pour simplifier. Vous pouvez remplacer cela par une structure personnalisée si nécessaire (par exemple, `data Container a = Container [a]`).
2. **Fonction `guessWhatsInside`** :
   - Prend un élément `item` de type `a` et un `Container a` (une liste).
   - Utilise la fonction `elem` de Haskell pour vérifier si `item` est dans la liste.
   - La contrainte `(Eq a)` garantit que l'élément peut être comparé avec `==`.
3. **Fonction `main`** :
   - Crée un `Container` de test avec des entiers `[1, 2, 3, 4, 5]`.
   - Teste la fonction `guessWhatsInside` avec deux éléments : `3` (présent) et `6` (absent).
   - Affiche les résultats dans la console.

### Exécution :
Si vous compilez et exécutez ce programme (par exemple, avec GHC), la sortie sera :

```
Container: [1,2,3,4,5]
Est-ce que 3 est dans le container ? True
Est-ce que 6 est dans le container ? False
```

### Remarques :
- Si vous aviez une définition spécifique de `Container` (par exemple, un type de données personnalisé comme `data Container a = Empty | Cons a (Container a)`), merci de préciser, et je peux adapter le code.
- Si vous voulez une version plus détaillée ou avec des fonctionnalités supplémentaires (par exemple, un container avec des types différents ou des tests plus complexes), faites-le moi savoir !
