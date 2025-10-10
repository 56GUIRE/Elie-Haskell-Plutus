
HC12T4 : Premiers 10 nombres de Fibonacci
```haskell
-- Définition de la fonction fibonacci
fibonacci :: Integer -> Integer
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- Fonction principale
main :: IO ()
main = do
    let fibs = [fibonacci n | n <- [0..9]]  -- Génère les 10 premiers nombres directement
    putStrLn "Les 10 premiers nombres de Fibonacci sont :"
    mapM_ (\(i, f) -> putStrLn $ show i ++ ": " ++ show f) (zip [0..] fibs)
```

### Explications détaillées :

1. **Définition de la fonction `fibonacci`** :
   - `fibonacci :: Integer -> Integer` :
     - Cette ligne déclare que `fibonacci` est une fonction qui prend un argument de type `Integer` (un entier potentiellement très grand) et retourne un `Integer`.
   - `fibonacci n` avec gardes :
     - `| n == 0 = 0` : Si `n` est 0, la fonction retourne 0 (premier nombre de Fibonacci).
     - `| n == 1 = 1` : Si `n` est 1, la fonction retourne 1 (deuxième nombre de Fibonacci).
     - `| otherwise = fibonacci (n - 1) + fibonacci (n - 2)` : Pour tout autre `n`, la fonction calcule la somme des deux nombres de Fibonacci précédents en s'appelant récursivement. Cela suit la définition mathématique de la suite de Fibonacci : F(n) = F(n-1) + F(n-2).
   - La récursivité s'arrête aux cas de base (0 et 1), ce qui permet au calcul de se dérouler correctement.

2. **Fonction principale `main`** :
   - `main :: IO ()` :
     - C'est le point d'entrée du programme en Haskell. Il est de type `IO ()`, ce qui signifie qu'il effectue des actions d'entrée/sortie (comme afficher du texte) et ne retourne rien (`()`).
   - `let fibs = [fibonacci n | n <- [0..9]]` :
     - Cela crée une liste appelée `fibs` en utilisant une **liste par compréhension**.
     - `[0..9]` génère la liste des entiers de 0 à 9 (10 nombres au total).
     - Pour chaque `n` dans `[0..9]`, `fibonacci n` calcule le n-ième nombre de Fibonacci, donnant ainsi les 10 premiers nombres : F(0), F(1), ..., F(9).
   - `putStrLn "Les 10 premiers nombres de Fibonacci sont :"` :
     - Affiche un message de titre dans le terminal.
   - `mapM_ (\(i, f) -> putStrLn $ show i ++ ": " ++ show f) (zip [0..] fibs)` :
     - `zip [0..] fibs` combine la liste des indices `[0..9]` avec la liste `fibs` pour créer des paires `(i, f)`, où `i` est l'index et `f` est le nombre de Fibonacci correspondant.
     - `\(i, f) -> putStrLn $ show i ++ ": " ++ show f` est une fonction anonyme qui, pour chaque paire `(i, f)`, convertit `i` et `f` en chaînes de caractères avec `show`, les concatène avec `": "` entre eux, et affiche le résultat avec `putStrLn`.
     - `mapM_` applique cette fonction à chaque paire et exécute l'affichage ligne par ligne.

3. **Pourquoi ça marche maintenant** :
   - Les erreurs précédentes (comme `Couldn't match expected type ‘t0 -> [Integer]’ with actual type ‘[Integer]’`) étaient dues à une mauvaise définition ou utilisation de `fibonacciList`, qui introduisait un conflit de type. En supprimant `fibonacciList` et en générant la liste directement avec une compréhension, on évite ce problème.
   - Le code est maintenant minimaliste, avec des types explicites (`Integer`) et une logique claire, ce qui le rend compatible avec la plupart des environnements Haskell, y compris votre éditeur en ligne ou mobile.


