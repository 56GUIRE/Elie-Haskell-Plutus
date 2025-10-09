HC11T1 : Instance WeAccept pour Box
```haskell
-- Définition de la classe WeAccept
class WeAccept a where
  isAccepted :: a -> Bool

-- Définition du type Box
data Box = Box Int deriving (Show)

-- Instance de WeAccept pour Box
instance WeAccept Box where
  isAccepted (Box n) = n > 0  -- Une boîte est acceptée si son contenu est positif

-- Fonction pour filtrer les boîtes acceptées
acceptedBoxes :: [Box] -> [Box]
acceptedBoxes boxes = filter isAccepted boxes

-- Fonction principale (main)
main :: IO ()
main = do
  let boxes = [Box 5, Box (-2), Box 10, Box 0, Box 3]  -- Exemple de liste de boîtes
  putStrLn "Boîtes initiales :"
  print boxes
  putStrLn "Boîtes acceptées :"
  print (acceptedBoxes boxes)
```

### Explications :
1. **Classe `WeAccept`** : La classe `WeAccept` est définie avec une méthode `isAccepted` qui prend une valeur de type `a` et renvoie un `Bool` indiquant si la valeur est acceptée.
2. **Type `Box`** : Le type `Box` est un simple conteneur pour un entier (`Int`). Il est défini avec `deriving (Show)` pour permettre l'affichage dans le `main`.
3. **Instance `WeAccept` pour `Box`** : Une boîte est considérée comme acceptée si son contenu (un entier) est strictement positif (`n > 0`).
4. **Fonction `acceptedBoxes`** : Cette fonction utilise `filter` pour ne garder que les boîtes pour lesquelles `isAccepted` renvoie `True`.
5. **Fonction `main`** : Le `main` crée une liste d'exemple de boîtes, affiche la liste initiale, puis affiche la liste des boîtes acceptées.

