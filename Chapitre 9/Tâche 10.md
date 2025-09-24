Voici le code Haskell pour définir un type de données récursif `BST a` représentant un arbre binaire de recherche (Binary Search Tree), avec des constructeurs pour un arbre vide et un nœud contenant une valeur de type `a` et deux sous-arbres. J'inclus également une fonction `main` pour tester ce type.

```haskell
-- Définition du type de données récursif BST (Binary Search Tree)
data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

-- Fonction main pour tester
main :: IO ()
main = do
  -- Création d'un arbre binaire de recherche
  -- Exemple :      5
  --              / \
  --             3   7
  --            / 
  --           1
  let bst = Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 7 Empty Empty)
  
  -- Affichage de l'arbre
  print bst
```

### Explications :
1. **Type `BST a`** :
   - `BST a` est un type de données récursif paramétré par `a`, représentant un arbre binaire de recherche.
   - Il a deux constructeurs :
     - `Empty` : représente un arbre vide (une feuille ou un sous-arbre vide).
     - `Node a (BST a) (BST a)` : représente un nœud avec une valeur de type `a`, un sous-arbre gauche (`BST a`), et un sous-arbre droit (`BST a`).
   - Le `deriving (Show)` permet d'afficher l'arbre de manière lisible pour le débogage.
2. **Fonction `main`** :
   - Crée un exemple d'arbre binaire de recherche avec les valeurs suivantes :
     - Racine : 5
     - Sous-arbre gauche : nœud avec 3, qui a un sous-arbre gauche contenant 1 et un sous-arbre droit vide.
     - Sous-arbre droit : nœud avec 7, avec deux sous-arbres vides.
   - Affiche l'arbre avec `print`.
3. **Structure de l'arbre** :
   - L'arbre respecte la propriété d'un BST : pour tout nœud, les valeurs dans le sous-arbre gauche sont inférieures à la valeur du nœud, et celles dans le sous-arbre droit sont supérieures (en supposant que `a` est ordonnable, bien que cela ne soit pas encore contraint ici).

### Résultat attendu :
En exécutant le programme, la sortie sera (formatée pour plus de clarté) :
```
Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 7 Empty Empty)
```

Ce code définit correctement un type d'arbre binaire de recherche. Si tu veux ajouter des contraintes (par exemple, `Ord a` pour garantir que les valeurs sont comparables) ou des fonctions (comme l'insertion ou la recherche dans l'arbre), ou si tu as besoin d'autres précisions, fais-le-moi savoir !
