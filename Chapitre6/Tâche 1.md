```haskell
-- Définition de la fonction factorielle
factorial :: Integer -> Integer
factorial 0 = 1              -- Cas de base : la factorielle de 0 est 1
factorial n = n * factorial (n - 1)  -- Cas récursif : n! = n * (n-1)!

-- Fonction principale
main :: IO ()
main = do
    let n = 5 :: Integer     -- On définit un nombre fixe (5) pour éviter les problèmes d'entrée
                            -- :: Integer spécifie le type pour s'assurer que c'est un entier
    if n < 0                 -- Condition pour vérifier si le nombre est négatif
        then putStrLn "Erreur : la factorielle n'est pas définie pour les nombres négatifs."
                            -- Si n est négatif, affiche un message d'erreur
        else print (factorial n)  -- Sinon, calcule et affiche la factorielle de n
```

### Explications détaillées :

1. **Déclaration de la fonction `factorial`** :
   - `factorial :: Integer -> Integer` : Cette ligne déclare que `factorial` prend un entier (`Integer`) comme entrée et retourne un entier.
   - `factorial 0 = 1` : C'est le cas de base de la récursion. La factorielle de 0 est définie comme 1 (convention mathématique).
   - `factorial n = n * factorial (n - 1)` : C'est le cas récursif. Pour calculer la factorielle de `n`, on multiplie `n` par la factorielle de `n-1`. Par exemple, pour `5!`, cela devient `5 * 4 * 3 * 2 * 1`.

2. **Fonction `main`** :
   - `main :: IO ()` : Cette fonction est le point d'entrée d'un programme Haskell. Elle effectue des actions d'entrée/sortie (`IO`) et ne retourne rien (`()`).
   - `let n = 5 :: Integer` : On assigne la valeur 5 à `n` de type `Integer`. Cela évite d'utiliser `getLine`, qui causait des problèmes dans ton environnement.
   - `if n < 0 then ... else ...` : Une condition qui vérifie si `n` est négatif. Si oui, elle affiche un message d'erreur, sinon elle calcule la factorielle.
   - `putStrLn` : Affiche une chaîne de caractères (ici, le message d'erreur).
   - `print (factorial n)` : Calcule la factorielle de `n` et l'affiche. Pour `n = 5`, cela donne `120` (car 5! = 5 * 4 * 3 * 2 * 1 = 120).

### Pourquoi ça marche maintenant ?
- En supprimant l'interaction avec `getLine`, on évite les problèmes liés à l'entrée dans certains éditeurs en ligne. Le programme exécute directement le calcul avec la valeur fixe.

### Que faire ensuite ?
- Si tu veux tester avec d'autres nombres, tu peux changer la valeur de `n` (par exemple, `let n = 6 :: Integer` pour obtenir 720).
- Pour revenir à une version interactive, tu devras utiliser un environnement local comme GHC, où tu pourras entrer des valeurs manuellement.

Si tu veux plus de détails ou une modification (par exemple, ajouter une boucle ou gérer des erreurs), fais-le-moi savoir !
