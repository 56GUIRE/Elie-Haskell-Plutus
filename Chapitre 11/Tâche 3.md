HC11T3 : Classe de type Container pour Box

```haskell
-- Définition de la classe de type Container
class Container c where
    isEmpty :: c a -> Bool
    contains :: Eq a => c a -> a -> Bool
    replace :: c a -> a -> c a

-- Définition du type de données Box
data Box a = Empty | Content a deriving (Show, Eq)

-- Implémentation de l'instance Container pour Box
instance Container Box where
    isEmpty Empty = True
    isEmpty (Content _) = False

    contains Empty _ = False
    contains (Content x) y = x == y

    replace _ x = Content x

-- Fonction principale pour démontrer l'utilisation
main :: IO ()
main = do
    let emptyBox = Empty :: Box Int
    let boxWith5 = Content 5 :: Box Int

    -- Test de isEmpty
    putStrLn $ "Is emptyBox empty? " ++ show (isEmpty emptyBox)
    putStrLn $ "Is boxWith5 empty? " ++ show (isEmpty boxWith5)

    -- Test de contains
    putStrLn $ "Does emptyBox contain 5? " ++ show (contains emptyBox 5)
    putStrLn $ "Does boxWith5 contain 5? " ++ show (contains boxWith5 5)
    putStrLn $ "Does boxWith5 contain 3? " ++ show (contains boxWith5 3)

    -- Test de replace
    let newBox = replace emptyBox 10
    let updatedBox = replace boxWith5 20
    putStrLn $ "After replacing emptyBox with 10: " ++ show newBox
    putStrLn $ "After replacing boxWith5 with 20: " ++ show updatedBox
```

### Explications :

1. **Classe de type `Container`** :
   - La classe `Container` est définie avec trois méthodes :
     - `isEmpty` : Vérifie si le conteneur est vide.
     - `contains` : Vérifie si un élément donné est contenu dans le conteneur (utilise la contrainte `Eq` pour permettre la comparaison).
     - `replace` : Remplace le contenu du conteneur par un nouvel élément.

2. **Type `Box`** :
   - `Box` est un type de données simple avec deux constructeurs :
     - `Empty` : Représente une boîte vide.
     - `Content a` : Représente une boîte contenant un élément de type `a`.
   - Le `deriving (Show, Eq)` permet d'afficher les boîtes et de comparer leur contenu.

3. **Instance de `Container` pour `Box`** :
   - `isEmpty` : Retourne `True` pour `Empty`, `False` pour `Content`.
   - `contains` : Retourne `False` pour `Empty`, et compare l'élément contenu dans `Content` avec l'élément donné.
   - `replace` : Remplace le contenu par un nouveau `Content x`, que la boîte soit vide ou non.

4. **Programme principal (`main`)** :
   - Crée une boîte vide (`emptyBox`) et une boîte contenant l'entier 5 (`boxWith5`).
   - Teste les méthodes `isEmpty`, `contains` et `replace` avec des exemples.
   - Affiche les résultats pour démontrer le fonctionnement.

