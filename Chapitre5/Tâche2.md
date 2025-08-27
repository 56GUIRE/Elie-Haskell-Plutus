HC5T2 : Filtrer les nombres détériore
Voici le code en Haskell qui utilise la fonction `filter` pour extraire tous les nombres impairs d'une liste d'entiers de 1 à 30, avec un `main` pour afficher le résultat :

```haskell
-- Fonction principale
main :: IO ()
main = do
    let numbers = [1..30]  -- Liste de 1 à 30
    let odds = filter odd numbers  -- Filtre les nombres impairs
    print odds  -- Affiche [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]
```

### Explications :
- `[1..30]` crée une liste d'entiers de 1 à 30.
- `filter odd numbers` utilise la fonction `odd` (prédéfini en Haskell, qui retourne `True` pour les nombres impairs) pour extraire les nombres impairs de la liste `numbers`.
- Le résultat est stocké dans `odds`, qui contient `[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]`.
- `print odds` affiche la liste des nombres impairs dans la console.

Ce code est simple, idiomatique en Haskell, et peut être exécuté dans GHC pour voir le résultat.
