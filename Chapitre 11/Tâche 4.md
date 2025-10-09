HC11T4 : Conteneur d'instance pour présent
```haskell
-- Définition de la classe Container
class Container c where
  isEmpty :: c a -> Bool
  getContent :: c a -> Maybe a

-- Définition du type de données Present
data Present a = Empty | Gift a deriving (Show)

-- Instance de Container pour Present
instance Container Present where
  isEmpty Empty = True
  isEmpty (Gift _) = False

  getContent Empty = Nothing
  getContent (Gift x) = Just x

-- Fonction principale pour tester l'implémentation
main :: IO ()
main = do
  let emptyPresent = Empty :: Present Int
      giftPresent = Gift 42

  -- Test de isEmpty
  putStrLn $ "Is empty present empty? " ++ show (isEmpty emptyPresent)
  putStrLn $ "Is gift present empty? " ++ show (isEmpty giftPresent)

  -- Test de getContent
  putStrLn $ "Content of empty present: " ++ show (getContent emptyPresent)
  putStrLn $ "Content of gift present: " ++ show (getContent giftPresent)
```

### Explication :
1. **Classe `Container`** :
   - `isEmpty` vérifie si le conteneur est vide.
   - `getContent` retourne le contenu sous forme de `Maybe a` pour gérer les cas où le conteneur est vide.

2. **Type `Present`** :
   - C'est un type de données avec deux constructeurs : `Empty` (pas de contenu) et `Gift a` (contient une valeur de type `a`).

3. **Instance de `Container` pour `Present`** :
   - `isEmpty` retourne `True` pour `Empty` et `False` pour `Gift`.
   - `getContent` retourne `Nothing` pour `Empty` et `Just x` pour `Gift x`.

4. **Main** :
   - Le programme crée deux valeurs `Present` : une vide (`Empty`) et une avec un contenu (`Gift 42`).
   - Il teste les fonctions `isEmpty` et `getContent` et affiche les résultats.
