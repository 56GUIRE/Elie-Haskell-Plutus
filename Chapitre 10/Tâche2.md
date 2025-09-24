exemple type Sommable :
```haskell
-- Définition de la classe de type Summable
class Summable a where
  sumUp :: [a] -> Maybe a

-- Instance de Summable pour Int
instance Summable Int where
  sumUp [] = Nothing          -- Liste vide : retourne Nothing
  sumUp xs = Just (sum xs)    -- Liste non vide : retourne la somme dans Just

-- Fonction main pour tester
main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5] :: [Int]  -- Type explicite ajouté
  let emptyList = [] :: [Int]              -- Type explicite
  print $ sumUp numbers                    -- Devrait afficher : Just 15
  print $ sumUp emptyList                  -- Devrait afficher : Nothing
```

### Explication détaillée :
1. **Classe `Summable`** :
   - La classe `Summable` définit une méthode `sumUp :: [a] -> Maybe a`, qui prend une liste de type `[a]` et retourne une valeur de type `Maybe a`. L'utilisation de `Maybe` permet de gérer proprement les cas où la liste est vide (`Nothing`) ou contient des éléments à sommer (`Just` avec le résultat).

2. **Instance `Summable Int`** :
   - Pour le type `Int`, l'instance spécifie :
     - `sumUp [] = Nothing` : Si la liste est vide, retourne `Nothing`, car il n'y a rien à sommer.
     - `sumUp xs = Just (sum xs)` : Si la liste contient des éléments, utilise la fonction `sum` (définie dans la bibliothèque standard pour les types `Num`) pour calculer la somme et l'enveloppe dans `Just`.
   - Cela nécessite que `Int` soit une instance de la classe `Num`, ce qui est vrai par défaut en Haskell.
