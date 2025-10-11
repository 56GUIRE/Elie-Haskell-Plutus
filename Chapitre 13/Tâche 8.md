HC13T8 : Importations qualifiées pour les conflits de noms
```haskell
import System.IO
import qualified Data.List as List
import qualified Data.Map as Map

-- Fonction pour filtrer une liste avec Data.List.filter
filterList :: [Int] -> [Int]
filterList xs = List.filter (> 0) xs

-- Fonction pour filtrer une map avec Data.Map.filter (simulée avec une liste convertie)
filterMap :: [(Int, String)] -> Map.Map Int String
filterMap pairs = Map.fromList $ List.filter (\(k, _) -> k > 0) pairs

-- Programme principal
main :: IO ()
main = do
  let listData = [-1, 0, 1, 2, 3]
  let filteredList = filterList listData
  putStrLn $ "Liste filtrée (valeurs > 0) : " ++ show filteredList

  let mapData = [(-1, "neg"), (0, "zero"), (1, "one"), (2, "two")]
  let filteredMap = filterMap mapData
  putStrLn $ "Map filtrée (clés > 0) : " ++ show (Map.toList filteredMap)
```

### Explication :

1. **Importations** :
   - `import System.IO` : Importe les fonctionnalités d'entrée/sortie de base (`putStrLn`) pour afficher les résultats.
   - `import qualified Data.List as List` : Importe le module `Data.List` avec un espace de noms qualifié `List`. Cela permet d'accéder à `filter` (et d'autres fonctions) via `List.filter`, évitant les conflits avec d'autres modules.
   - `import qualified Data.Map as Map` : Importe le module `Data.Map` avec un espace de noms qualifié `Map`. Cela permet d'accéder à `fromList`, `toList`, et `filter` via `Map.fromList`, etc. La qualification est essentielle car `Data.Map` contient aussi une fonction `filter`, qui pourrait entrer en conflit avec `Data.List.filter`.

2. **Fonction `filterList`** :
   - `filterList :: [Int] -> [Int]` : Prend une liste d'entiers et retourne une nouvelle liste contenant uniquement les valeurs positives (> 0).
   - `List.filter (> 0) xs` : Utilise `List.filter` pour appliquer le prédicat `> 0` à chaque élément de `xs`. La qualification `List.` évite tout conflit avec une autre fonction `filter`.

3. **Fonction `filterMap`** :
   - `filterMap :: [(Int, String)] -> Map.Map Int String` : Prend une liste de paires `(Int, String)` et retourne une map filtrée où seules les paires avec une clé positive (> 0) sont conservées.
   - `Map.fromList $ List.filter (\(k, _) -> k > 0) pairs` : Filtre la liste de paires avec `List.filter` en vérifiant la clé (`k`), puis convertit le résultat en une map avec `Map.fromList`. La qualification `Map.` et `List.` garantit qu'il n'y a pas d'ambiguïté, même si `Data.Map` a sa propre `filter`.

4. **Fonction `main`** :
   - `let listData = [-1, 0, 1, 2, 3]` : Définit une liste de test avec des nombres positifs et négatifs.
   - `let filteredList = filterList listData` : Applique `filterList` et stocke le résultat.
   - `putStrLn $ "Liste filtrée (valeurs > 0) : " ++ show filteredList` : Affiche la liste filtrée (par exemple, `[1, 2, 3]`).
   - `let mapData = [(-1, "neg"), (0, "zero"), (1, "one"), (2, "two")]` : Définit une liste de paires pour tester la map.
   - `let filteredMap = filterMap mapData` : Applique `filterMap` et stocke le résultat.
   - `putStrLn $ "Map filtrée (clés > 0) : " ++ show (Map.toList filteredMap)` : Affiche la map convertie en liste de paires (par exemple, `[(1, "one"), (2, "two")]`).

  
