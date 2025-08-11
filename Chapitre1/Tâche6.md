HC1T6 - Tâche 6 : Utilisation de signatures de type:

```haskell
module Main where

-- Additionne deux entiers
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Fonction principale pour tester
main :: IO ()
main = do
  let a = 5
      b = 3
  print $ addNumbers a b
```

### Explications :
1. **addNumbers** :
   - Prend deux paramètres de type `Int` et retourne leur somme (type `Int`).
   - Utilise l'opérateur `+` pour effectuer l'addition.

2. **main** :
   - Définit deux entiers `a = 5` et `b = 3` pour tester la fonction.
   - Appelle `addNumbers a b` et affiche le résultat avec `print`.

### Sortie attendue :
Pour `a = 5` et `b = 3`, la sortie sera :
```haskell
8
```

### Remarques :
- La fonction est simple et type-sûre grâce au système de types de Haskell.
- Vous pouvez modifier les valeurs de `a` et `b` dans le `main` pour tester d'autres cas.
- Si vous voulez que la fonction accepte d'autres types numériques (comme `Double`), vous pouvez utiliser une classe de types comme `Num`: `addNumbers :: Num a => a -> a -> a`.
