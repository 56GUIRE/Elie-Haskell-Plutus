HC6T2 : Suite de Fibonacci (récursif)

```haskell
-- Définition de la fonction Fibonacci
fib :: Integer -> Integer
fib 0 = 0              -- Cas de base : F(0) = 0
fib 1 = 1              -- Cas de base : F(1) = 1
fib n = fib (n - 1) + fib (n - 2)  -- Cas récursif : F(n) = F(n-1) + F(n-2)

-- Fonction principale
main :: IO ()
main = do
    let n = 10 :: Integer  -- Nombre fixe pour tester (par exemple, le 10e nombre de Fibonacci)
    if n < 0
        then putStrLn "Erreur : l'index doit être un nombre non négatif."
        else print (fib n)  -- Affiche le nième nombre de Fibonacci
```

### Explications :
1. **Fonction `fib`** :
   - `fib :: Integer -> Integer` : Déclare que `fib` prend un entier (`Integer`) comme entrée et retourne un entier.
   - `fib 0 = 0` : Cas de base, le 0e nombre de la suite est 0.
   - `fib 1 = 1` : Cas de base, le 1er nombre de la suite est 1.
   - `fib n = fib (n - 1) + fib (n - 2)` : Cas récursif, où chaque nombre est la somme des deux précédents. Par exemple, pour \( F(10) \), cela calcule \( F(9) + F(8) \), et ainsi de suite.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let n = 10 :: Integer` : Définit un index fixe (10) pour tester la fonction sans entrée interactive.
   - `if n < 0 then ... else ...` : Vérifie si l'index est négatif (la suite de Fibonacci n'est pas définie pour les indices négatifs).
   - `print (fib n)` : Calcule et affiche le nième nombre de Fibonacci. Pour \( n = 10 \), le résultat est 55 (car la suite commence par 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).

