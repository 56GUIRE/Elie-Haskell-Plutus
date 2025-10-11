HC13T9 : Renommer un espace de noms de module
```haskell
import System.IO
import qualified Data.List as L
import qualified Data.Map as M

-- Fonction pour trier une liste avec Data.List.sort
sortList :: [Int] -> [Int]
sortList xs = L.sort xs

-- Fonction pour convertir une liste de paires en map et appliquer une transformation
transformMap :: [(Int, String)] -> M.Map Int String
transformMap pairs = M.fromList pairs

-- Programme principal
main :: IO ()
main = do
  let listData = [3, 1, 4, 1, 5]  -- Liste de nombres pour tester
  let sortedList = sortList listData
  putStrLn $ "Liste triée : " ++ show sortedList

  let mapData = [(1, "one"), (2, "two"), (3, "three")]  -- Liste de paires pour tester
  let transformedMap = transformMap mapData
  putStrLn $ "Map transformée : " ++ show (M.toList transformedMap)
```

### Explication :

1. **Importations** :
   - `import System.IO` : Importe les fonctionnalités d'entrée/sortie de base (`putStrLn`) pour afficher les résultats.
   - `import qualified Data.List as L` : Importe le module `Data.List` et renomme son espace de noms en `L`. Cela permet d'accéder à des fonctions comme `sort` via `L.sort`, suivant l'exemple de renommage dans ton cours pour rendre le code plus lisible.
   - `import qualified Data.Map as M` : Importe le module `Data.Map` et renomme son espace de noms en `M`. Cela permet d'accéder à des fonctions comme `fromList` et `toList` via `M.fromList` et `M.toList`, évitant les conflits et simplifiant l'écriture.

2. **Fonction `sortList`** :
   - `sortList :: [Int] -> [Int]` : Prend une liste d'entiers et retourne une nouvelle liste triée par ordre croissant.
   - `L.sort xs` : Utilise la fonction `sort` de `Data.List` (accédée via l'espace de noms renommé `L`) pour trier la liste. Le renommage en `L` rend le code plus concis tout en restant clair.

3. **Fonction `transformMap`** :
   - `transformMap :: [(Int, String)] -> M.Map Int String` : Prend une liste de paires `(Int, String)` et retourne une map.
   - `M.fromList pairs` : Convertit la liste de paires en une map en utilisant `M.fromList` (de `Data.Map`, via l'espace de noms renommé `M`). Cela montre comment utiliser une fonction d'un module renommé.

4. **Fonction `main`** :
   - `let listData = [3, 1, 4, 1, 5]` : Définit une liste de test avec des nombres non triés.
   - `let sortedList = sortList listData` : Applique `sortList` et stocke le résultat.
   - `putStrLn $ "Liste triée : " ++ show sortedList` : Affiche la liste triée (par exemple, `[1, 1, 3, 4, 5]`).
   - `let mapData = [(1, "one"), (2, "two"), (3, "three")]` : Définit une liste de paires pour tester la map.
   - `let transformedMap = transformMap mapData` : Applique `transformMap` et stocke le résultat.
   - `putStrLn $ "Map transformée : " ++ show (M.toList transformedMap)` : Affiche la map convertie en liste de paires (par exemple, `[(1, "one"), (2, "two"), (3, "three")]`), en utilisant `M.toList` pour la lisibilité.

   
