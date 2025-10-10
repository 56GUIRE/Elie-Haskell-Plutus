Voici un programme Haskell qui définit une fonction `addTwoNumbers` prenant deux entiers en entrée et retournant leur somme, avec un `main` qui affiche le résultat :

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

3. **Sortie attendue** :
   En exécutant ce programme, la sortie sera :
   ```
   La somme de 5 et 3 est : 8
   ```

### Comment exécuter :
1. Sauvegardez le code dans un fichier, par exemple `AddTwoNumbers.hs`.
2. Compilez avec GHC : `ghc AddTwoNumbers.hs`.
3. Exécutez : `./AddTwoNumbers` (Linux/Mac) ou `AddTwoNumbers.exe` (Windows).

### Variante avec entrée utilisateur :
Si vous souhaitez que l'utilisateur entre les deux nombres, voici une version alternative :

```haskell
-- Définition de la fonction addTwoNumbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- Fonction principale avec entrée utilisateur
main :: IO ()
main = do
    putStrLn "Entrez le premier nombre : "
    input1 <- getLine
    putStrLn "Entrez le deuxième nombre : "
    input2 <- getLine
    let num1 = read input1 :: Int
    let num2 = read input2 :: Int
    let result = addTwoNumbers num1 num2
    putStrLn $ "La somme de " ++ show num1 ++ " et " ++ show num2 ++ " est : " ++ show result
```

### Explications de la variante :
- `getLine` lit une ligne de texte entrée par l'utilisateur.
- `read input :: Int` convertit la chaîne entrée en un entier.
- Le programme demande deux nombres, calcule leur somme avec `addTwoNumbers`, et affiche le résultat.

### Sortie attendue (variante) :
Pour les entrées `5` et `3` :
```
Entrez le premier nombre : 
5
Entrez le deuxième nombre : 
3
La somme de 5 et 3 est : 8
```

Choisissez la version qui correspond le mieux à vos besoins. Si vous avez des questions ou souhaitez des modifications, dites-le-moi !
