HC1T5 - Tâche 5 : Paresse en Haskell:

```haskell
module Main where

-- Génère une liste infinie de nombres (à partir de 1)
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

-- Extrait les n premiers éléments de la liste infinie
takeFirstN :: Int -> [Int]
takeFirstN n = take n infiniteNumbers

-- Fonction principale pour tester
main :: IO ()
main = do
  let n = 5
  print $ takeFirstN n
```

### Explications :
1. **infiniteNumbers** :
   - Utilise la syntaxe Haskell `[1..]` pour générer une liste infinie de nombres entiers à partir de 1 (1, 2, 3, ...).
   - Grâce à l'évaluation paresseuse de Haskell, la liste infinie n'est générée qu'à la demande.

2. **takeFirstN** :
   - Prend un entier `n` et utilise la fonction standard `take` pour extraire les `n` premiers éléments de `infiniteNumbers`.
   - Retourne une liste finie de longueur `n`.

3. **main** :
   - Teste la fonction `takeFirstN` avec `n = 5`.
   - Affiche le résultat à l'aide de `print`.

### Sortie attendue :
Pour `n = 5`, la sortie sera :
```haskell
[1,2,3,4,5]
```

### Remarques :
- La liste infinie est gérée efficacement grâce à l'évaluation paresseuse de Haskell, qui ne calcule que les éléments nécessaires.
- Vous pouvez modifier la valeur de `n` dans le `main` pour extraire un nombre différent d'éléments.
- Si vous voulez une liste infinie commençant à un autre nombre ou avec un pas différent, vous pouvez ajuster `infiniteNumbers`, par exemple `[0,2..]` pour les nombres pairs.
