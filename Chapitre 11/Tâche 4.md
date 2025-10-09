Pour implémenter la classe `Container` pour le type `Present` en Haskell avec un `main`, il faut d'abord clarifier ce que représente le type `Present` et la classe `Container`. Comme vous n'avez pas fourni la définition exacte de la classe `Container` ou du type `Present`, je vais supposer une définition classique et simple pour illustrer l'implémentation. Voici une hypothèse raisonnable :

- Le type `Present` est un type de données qui peut contenir une valeur, comme une boîte cadeau contenant un élément ou rien (similaire à `Maybe`).
- La classe `Container` définit des opérations pour manipuler le contenu d'un conteneur, comme vérifier s'il est vide ou accéder à son contenu.

Voici une implémentation en Haskell avec ces hypothèses :

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

### Sortie attendue :
En exécutant ce programme (par exemple, avec `ghc` ou dans GHCi), la sortie sera :
```
Is empty present empty? True
Is gift present empty? False
Content of empty present: Nothing
Content of gift present: Just 42
```

### Remarques :
- Si vous avez une définition spécifique de la classe `Container` ou du type `Present`, veuillez la fournir pour que je puisse adapter le code.
- Si vous voulez ajouter d'autres méthodes à la classe `Container` ou modifier le comportement, indiquez-le, et je mettrai à jour l'implémentation.

Vous pouvez compiler et exécuter ce code avec GHC ou le tester dans GHCi. Si vous avez besoin d'aide pour exécuter ou modifier le code, faites-le-moi savoir !
