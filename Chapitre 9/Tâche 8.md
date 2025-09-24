Voici le code Haskell pour définir un type de données récursif `Sequence a` représentant une séquence linéaire de nœuds, où chaque nœud contient une valeur de type `a` et pointe vers le nœud suivant (ou rien si c'est la fin). J'inclus également une fonction `main` pour tester ce type.

```haskell
-- Définition du type de données récursif Sequence
data Sequence a = Empty | Node a (Sequence a) deriving (Show)

-- Fonction main pour tester
main :: IO ()
main = do
  -- Création d'une séquence : 1 -> 2 -> 3 -> Empty
  let seq = Node 1 (Node 2 (Node 3 Empty))
  
  -- Affichage de la séquence
  print seq  -- Devrait afficher : Node 1 (Node 2 (Node 3 Empty))
```

### Explications :
1. **Type `Sequence a`** :
   - `Sequence a` est un type de données récursif paramétré par `a`.
   - Il a deux constructeurs :
     - `Empty` : représente la fin de la séquence (équivalent à une liste vide).
     - `Node a (Sequence a)` : représente un nœud contenant une valeur de type `a` et une référence au nœud suivant (de type `Sequence a`).
   - Le `deriving (Show)` permet d'afficher la séquence de manière lisible.
2. **Fonction `main`** :
   - Crée une séquence contenant les valeurs 1, 2 et 3, dans cet ordre, terminée par `Empty`.
   - Affiche la séquence avec `print`.

### Résultat attendu :
En exécutant le programme, la sortie sera :
```
Node 1 (Node 2 (Node 3 Empty))
```

Ce code définit correctement une séquence linéaire récursive, similaire à une liste chaînée. Si tu veux ajouter des fonctions pour manipuler la séquence (par exemple, ajouter un élément, calculer la longueur, etc.), ou si tu as besoin d'autres précisions, fais-le-moi savoir !
