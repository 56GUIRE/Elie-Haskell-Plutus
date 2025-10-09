HC11T10 : Fonction sortContainers
```haskell
-- Définition du type de données Present avec dérivation de Ord
data Present a = Empty | Gift a deriving (Show, Eq, Ord)

-- Fonction pour trier une liste de conteneurs
sortContainers :: (Ord a) => [Present a] -> [Present a]
sortContainers = sort
  where
    sort = foldr insert []
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

-- Fonction principale pour tester sortContainers
main :: IO ()
main = do
  let presents = [Gift 5, Empty, Gift 2, Gift 10, Empty, Gift 1] :: [Present Int]
  putStrLn "Liste non triée :"
  print presents
  putStrLn "Liste triée :"
  print (sortContainers presents)
```

### Explication :
1. **Type `Present`** :
   - Le type `Present a` est défini avec deux constructeurs : `Empty` et `Gift a`.
   - La dérivation de `Ord` (`deriving Ord`) définit automatiquement un ordre lexicographique : `Empty` est inférieur à tout `Gift a`, et les `Gift a` sont comparés selon la valeur `a` (qui doit être une instance de `Ord`).

2. **Fonction `sortContainers`** :
   - La fonction prend une liste de `[Present a]` et retourne une liste triée.
   - J'ai implémenté un tri par insertion simple pour illustrer le processus, bien que la bibliothèque standard Haskell propose `Data.List.sort`. L'implémentation personnalisée utilise `foldr` et une fonction auxiliaire `insert` pour insérer chaque élément dans une liste triée.
   - La contrainte `(Ord a)` garantit que la valeur contenue dans `Gift a` peut être comparée.

3. **Main** :
   - Le programme crée une liste de `Present Int` avec des valeurs variées (`Empty` et `Gift` avec différents nombres).
   - Il affiche la liste non triée, puis la liste triée après application de `sortContainers`.

