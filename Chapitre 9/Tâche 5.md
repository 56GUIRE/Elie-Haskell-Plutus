Voici le code Haskell pour définir un type de données paramétré `Shape a` avec deux constructeurs, `Circle` et `Rectangle`, chacun contenant un champ `color` de type `a`. J'inclus également une fonction `main` pour démontrer l'utilisation de ce type.

```haskell
-- Définition du type de données paramétré Shape
data Shape a = Circle a | Rectangle a deriving (Show)

-- Fonction main pour tester
main :: IO ()
main = do
    let circle = Circle "Blue"        -- Un cercle avec une couleur de type String
    let rectangle = Rectangle "Red"   -- Un rectangle avec une couleur de type String
    print circle                      -- Affiche: Circle "Blue"
    print rectangle                   -- Affiche: Rectangle "Red"
```

### Explications :
1. **Type `Shape a`** :
   - `Shape` est un type de données paramétré qui prend un paramètre de type `a`.
   - Il a deux constructeurs : `Circle` et `Rectangle`, chacun prenant une valeur de type `a` pour représenter la couleur (`color`).
   - Le `deriving (Show)` permet d'afficher les valeurs de `Shape a` de manière lisible.
2. **Fonction `main`** :
   - Crée deux formes : un `Circle` avec la couleur `"Blue"` et un `Rectangle` avec la couleur `"Red"`.
   - Utilise `print` pour afficher ces formes dans la console.

### Résultat attendu :
En exécutant le programme, la sortie sera :
```
Circle "Blue"
Rectangle "Red"
```

Ce code est simple et respecte les conventions Haskell. Si tu veux ajouter des fonctionnalités (comme des fonctions pour manipuler les formes) ou si tu as des questions, fais-le-moi savoir !
