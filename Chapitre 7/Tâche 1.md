Voici un code Haskell qui définit un type de données `Color` représentant les couleurs Red, Green et Blue, implémente la classe de type `Eq` pour permettre la comparaison d'égalité entre deux couleurs, et inclut une fonction `main` pour tester.

```haskell
-- Définition du type de données Color
data Color = Red | Green | Blue deriving (Show, Eq)

-- Fonction principale
main :: IO ()
main = do
    let c1 = Red
    let c2 = Green
    let c3 = Red
    print (c1 == c2)  -- Affiche False (Red != Green)
    print (c1 == c3)  -- Affiche True (Red == Red)
```

### Explications :
1. **Définition du type de données `Color`** :
   - `data Color = Red | Green | Blue` : Définit un type algébrique `Color` avec trois constructeurs : `Red`, `Green`, et `Blue`. Ces constructeurs représentent les trois couleurs possibles.
   - `deriving (Show, Eq)` : 
     - `Show` permet d'afficher les valeurs du type (par exemple, avec `print`).
     - `Eq` permet de comparer les valeurs pour l'égalité (par exemple, avec `==`). En dérivant `Eq`, Haskell génère automatiquement une implémentation d'égalité qui considère deux couleurs comme égales si elles ont le même constructeur (par exemple, `Red == Red`).

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let c1 = Red` : Définit une variable `c1` comme étant de couleur `Red`.
   - `let c2 = Green` : Définit une variable `c2` comme étant de couleur `Green`.
   - `let c3 = Red` : Définit une variable `c3` comme étant de couleur `Red`.
   - `print (c1 == c2)` : Compare `c1` et `c2`. Puisque `Red != Green`, cela affiche `False`.
   - `print (c1 == c3)` : Compare `c1` et `c3`. Puisque `Red == Red`, cela affiche `True`.

### Remarques :
- L'implémentation de `Eq` est automatique grâce à `deriving`, mais elle fonctionne uniquement pour comparer des valeurs du même type. Par exemple, tu ne peux pas comparer un `Color` avec un autre type.
- Pour tester avec d'autres combinaisons, modifie les valeurs de `c1`, `c2`, ou `c3` (par exemple, `let c2 = Blue`).
- Si tu veux ajouter des fonctionnalités (comme convertir les couleurs en chaînes ou les utiliser dans d'autres fonctions), fais-le-moi savoir !

Copie ce code dans ton éditeur et exécute-le. Le résultat sera :
```
False
True
```

Si tu as des questions ou veux des ajustements, dis-le-moi !
