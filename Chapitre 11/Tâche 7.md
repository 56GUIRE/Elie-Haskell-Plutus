HC11T7 : Instance Ord pour Box
```haskell
-- Définition du type Box
data Box = Box
  { size :: Double  -- Taille ou volume de la boîte
  } deriving (Show)

-- Instance Eq pour Box (nécessaire pour Ord)
instance Eq Box where
  (Box size1) == (Box size2) = size1 == size2

-- Instance Ord pour Box
instance Ord Box where
  compare (Box size1) (Box size2) = compare size1 size2

-- Fonction main pour tester
main :: IO ()
main = do
  let box1 = Box 10.0
  let box2 = Box 20.0
  let box3 = Box 10.0

  putStrLn "Boîtes définies :"
  putStrLn $ "box1 = " ++ show box1
  putStrLn $ "box2 = " ++ show box2
  putStrLn $ "box3 = " ++ show box3

  putStrLn "\nComparaisons avec compare :"
  putStrLn $ "box1 `compare` box2 = " ++ show (box1 `compare` box2) -- LT
  putStrLn $ "box2 `compare` box1 = " ++ show (box2 `compare` box1) -- GT
  putStrLn $ "box1 `compare` box3 = " ++ show (box1 `compare` box3) -- EQ

  putStrLn "\nTests d'ordre :"
  putStrLn $ "box1 < box2 = " ++ show (box1 < box2) -- True
  putStrLn $ "box2 > box1 = " ++ show (box2 > box1) -- True
  putStrLn $ "box1 == box3 = " ++ show (box1 == box3) -- True
```

### Explications :
1. **Type `Box`** :
   - Défini comme une structure avec un champ `size` de type `Double`, représentant une mesure (par exemple, le volume ou une dimension).
   - Le `deriving (Show)` permet d'afficher les boîtes facilement.

2. **Instance `Eq`** :
   - Nécessaire car `Ord` dépend de `Eq`.
   - Deux boîtes sont considérées égales si leurs tailles (`size`) sont égales.

3. **Instance `Ord`** :
   - Implémente la méthode `compare` en comparant les tailles des boîtes (`size1` et `size2`) à l'aide de la fonction `compare` standard de Haskell.
   - La fonction `compare` retourne `LT` (less than), `EQ` (equal), ou `GT` (greater than) selon l'ordre des tailles.
   - Cela permet d'utiliser des opérateurs comme `<`, `>`, `<=`, etc., automatiquement.

4. **Fonction `main`** :
   - Crée trois boîtes : `box1` (taille 10.0), `box2` (taille 20.0), et `box3` (taille 10.0).
   - Affiche les boîtes pour référence.
   - Teste la fonction `compare` avec différentes paires de boîtes.
   - Montre l'utilisation des opérateurs d'ordre (`<`, `>`, `==`) pour démontrer que l'instance `Ord` fonctionne.
