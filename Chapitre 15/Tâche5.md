Voici un programme Haskell avec une fonction de division sécurisée utilisant le type `Maybe` et incluant un `main` pour démontrer son utilisation :

```haskell
-- Définition de la fonction de division sécurisée
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing  -- Cas de division par zéro
safeDiv x y = Just (x `div` y)  -- Cas normal

-- Fonction main pour tester safeDiv
main :: IO ()
main = do
    putStrLn "Test de la division sécurisée :"
    print $ safeDiv 10 2   -- Devrait afficher Just 5
    print $ safeDiv 10 0   -- Devrait afficher Nothing
    print $ safeDiv 15 3   -- Devrait afficher Just 5
```

### Explications :
1. **Type `Maybe`** : La fonction `safeDiv` retourne `Maybe Int`, qui peut être soit `Just n` (où `n` est le résultat de la division) soit `Nothing` (en cas de division par zéro).
2. **Fonction `safeDiv`** : Prend deux entiers (`x` et `y`). Si `y` est 0, elle retourne `Nothing`. Sinon, elle retourne `Just (x `div` y)`.
3. **Fonction `main`** : Utilise `print` pour afficher les résultats de plusieurs tests de `safeDiv` dans la console.

### Résultat attendu lors de l'exécution :
```
Test de la division sécurisée :
Just 5
Nothing
Just 5
```

Ce programme est concis, fonctionnel et gère proprement les cas d'erreur avec `Maybe`.
