HC12T2 : Additionner deux nombres

```haskell
-- Définition de la fonction addTwoNumbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- Fonction principale pour afficher le résultat
main :: IO ()
main = do
    let result = addTwoNumbers 5 3  -- Exemple avec 5 et 3
    putStrLn $ "La somme de 5 et 3 est : " ++ show result
```

### Explications :
1. **Fonction `addTwoNumbers`** :
   - Prend deux paramètres de type `Int` et retourne leur somme (type `Int`).
   - La syntaxe `x + y` effectue l'addition des deux entiers.

2. **Fonction `main`** :
   - Utilise `let` pour calculer la somme de deux nombres (ici, 5 et 3 comme exemple).
   - `putStrLn` affiche le résultat dans le terminal.
   - `show result` convertit le résultat (un `Int`) en une chaîne de caractères pour l'afficher.

