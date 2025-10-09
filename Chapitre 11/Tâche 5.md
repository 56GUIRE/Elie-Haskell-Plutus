

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

