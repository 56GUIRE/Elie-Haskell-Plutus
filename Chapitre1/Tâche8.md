Voici le code Haskell pour la fonction `applyTwice` avec un programme principal :

```haskell
module Main where

-- Définition de la fonction applyTwice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Fonction main pour tester applyTwice
main :: IO ()
main = do
    -- Exemple 1 : Appliquer (+3) deux fois à 5
    let result1 = applyTwice (+3) 5
    print $ "applyTwice (+3) 5 = " ++ show result1
    
    -- Exemple 2 : Appliquer (*2) deux fois à 4
    let result2 = applyTwice (*2) 4
    print $ "applyTwice (*2) 4 = " ++ show result2
    
    -- Exemple 3 : Appliquer une fonction lambda deux fois
    let result3 = applyTwice (\x -> x + 1) 10
    print $ "applyTwice (\\x -> x + 1) 10 = " ++ show result3
```

Explications :
- `applyTwice` prend une fonction `f` de type `(a -> a)` (une fonction qui transforme une valeur de type `a` en une autre valeur du même type) et une valeur initiale `x` de type `a`.
- Elle applique la fonction `f` deux fois à `x`, c'est-à-dire `f (f x)`.
- Le programme principal `main` teste la fonction avec trois exemples :
  1. Ajoute 3 deux fois à 5 (résultat : 11)
  2. Multiplie par 2 deux fois 4 (résultat : 16)
  3. Applique une fonction lambda qui incrémente de 1 deux fois à 10 (résultat : 12)

Pour exécuter ce programme, vous pouvez le compiler avec GHC ou l'exécuter dans GHCi. La sortie sera :
```
applyTwice (+3) 5 = 11
applyTwice (*2) 4 = 16
applyTwice (\x -> x + 1) 10 = 12
```
