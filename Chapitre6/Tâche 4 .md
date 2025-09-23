Voici un code Haskell qui implémente une fonction récursive pour calculer la factorielle d'un nombre, avec une fonction `main` incluse. La factorielle de \( n \) (notée \( n! \)) est le produit de tous les entiers positifs de 1 à \( n \), avec \( 0! = 1 \).

```haskell
-- Définition de la fonction factorielle
factorial :: Integer -> Integer
factorial 0 = 1              -- Cas de base : la factorielle de 0 est 1
factorial n = n * factorial (n - 1)  -- Cas récursif : n! = n * (n-1)!

-- Fonction principale
main :: IO ()
main = do
    let n = 5 :: Integer  -- Nombre fixe pour tester (par exemple, 5!)
    if n < 0
        then putStrLn "Erreur : la factorielle n'est pas définie pour les nombres négatifs."
        else print (factorial n)  -- Affiche la factorielle de n
```

### Explications :
1. **Fonction `factorial`** :
   - `factorial :: Integer -> Integer` : Déclare que `factorial` prend un entier (`Integer`) comme entrée et retourne un entier.
   - `factorial 0 = 1` : Cas de base, la factorielle de 0 est 1 par définition.
   - `factorial n = n * factorial (n - 1)` : Cas récursif, où la factorielle de \( n \) est \( n \) multiplié par la factorielle de \( n-1 \). Par exemple, pour \( 5! \), cela donne \( 5 * 4 * 3 * 2 * 1 = 120 \).

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let n = 5 :: Integer` : Définit un nombre fixe (5) pour tester sans entrée interactive.
   - `if n < 0 then ... else ...` : Vérifie si \( n \) est négatif (la factorielle n'est pas définie pour les nombres négatifs).
   - `print (factorial n)` : Calcule et affiche la factorielle. Pour \( n = 5 \), le résultat est 120.

### Remarques :
- Cette version utilise un nombre fixe pour éviter les problèmes d'entrée/sortie rencontrés précédemment. Pour une version interactive avec `getLine`, tu peux me le demander, et je l'adapterai.
- Pour tester avec un autre nombre, change la valeur de `n` (par exemple, `let n = 6` pour obtenir 720).

Copie ce code dans ton éditeur et exécute-le. Si tu as des questions ou veux des ajustements, fais-le-moi savoir !
