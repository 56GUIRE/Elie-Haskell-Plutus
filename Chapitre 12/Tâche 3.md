Voici un programme Haskell qui définit une fonction `factorial` pour calculer la factorielle d'un entier positif donné, avec une fonction `main` pour afficher un exemple de résultat :

```haskell
-- Définition de la fonction factorielle
factorial :: Integer -> Integer
factorial n
    | n < 0     = error "La factorielle est définie seulement pour les entiers positifs ou zéro"
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

-- Fonction principale pour afficher le résultat
main :: IO ()
main = do
    let n = 5  -- Exemple avec n = 5
    putStrLn $ "La factorielle de " ++ show n ++ " est : " ++ show (factorial n)
```

### Explications :
1. **Fonction `factorial`** :
   - Prend un paramètre de type `Integer` (utilisé pour gérer de grands nombres, contrairement à `Int` qui est limité).
   - Utilise des gardes (`|`) pour gérer différents cas :
     - Si `n < 0`, une erreur est levée, car la factorielle n'est pas définie pour les nombres négatifs.
     - Si `n == 0`, retourne `1` (cas de base).
     - Sinon, calcule `n * factorial (n - 1)` (récursivité).
   - La récursivité s'arrête lorsque `n` atteint 0.

2. **Fonction `main`** :
   - Utilise `let` pour définir un exemple avec `n = 5`.
   - Affiche le résultat en combinant des chaînes avec `show` pour convertir l'entier en texte.

3. **Sortie attendue** :
   Pour `n = 5`, la sortie sera :
   ```
   La factorielle de 5 est : 120
   ```
   (Car 5! = 5 × 4 × 3 × 2 × 1 = 120)

### Variante avec entrée utilisateur :
Si vous préférez que l'utilisateur entre un nombre, voici une version alternative :

```haskell
-- Définition de la fonction factorielle
factorial :: Integer -> Integer
factorial n
    | n < 0     = error "La factorielle est définie seulement pour les entiers positifs ou zéro"
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

-- Fonction principale avec entrée utilisateur
main :: IO ()
main = do
    putStrLn "Entrez un entier positif ou zéro : "
    input <- getLine
    let n = read input :: Integer
    putStrLn $ "La factorielle de " ++ show n ++ " est : " ++ show (factorial n)
```

### Explications de la variante :
- `getLine` lit l'entrée de l'utilisateur sous forme de chaîne.
- `read input :: Integer` convertit la chaîne en un `Integer`.
- Le programme demande un nombre, calcule sa factorielle, et affiche le résultat.
- Si l'utilisateur entre un nombre négatif, le programme lève une erreur.

### Sortie attendue (variante) :
Pour l'entrée `5` :
```
Entrez un entier positif ou zéro : 
5
La factorielle de 5 est : 120
```

### Comment exécuter :
1. Sauvegardez le code dans un fichier, par exemple `Factorial.hs`.
2. Compilez avec GHC : `ghc Factorial.hs`.
3. Exécutez : `./Factorial` (Linux/Mac) ou `Factorial.exe` (Windows).

Choisissez la version qui vous convient. Si vous avez besoin d'autres ajustements ou d'explications, faites-le-moi savoir !
