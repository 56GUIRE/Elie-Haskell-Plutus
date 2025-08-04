Voici le code Haskell qui répond à votre demande, en utilisant la notation préfixe pour les expressions données et la notation infixe pour les fonctions spécifiées, avec une fonction `main` pour afficher les résultats :

```haskell
-- Fonction principale
main :: IO ()
main = do
  -- Notation préfixe pour les expressions
  putStrLn $ "5 + 3 en notation préfixe : " ++ show ((+) 5 3)
  putStrLn $ "10 * 4 en notation préfixe : " ++ show ((*) 10 4)
  putStrLn $ "Vrai et Faux en notation préfixe : " ++ show ((&&) True False)
  
  -- Notation infixe pour les fonctions
  putStrLn $ "(+) 7 2 en notation infixe : " ++ show (7 + 2)
  putStrLn $ "(*) 6 5 en notation infixe : " ++ show (6 * 5)
  putStrLn $ "(&&) Vrai Faux en notation infixe : " ++ show (True && False)
```

### Explications

- **Notation Préfixe** :
  - En Haskell, les opérateurs comme `+`, `*`, et `&&` peuvent être utilisés en notation préfixe en les entourant de parenthèses. Par exemple, `(+)` devient une fonction qui prend deux arguments.
  - Les expressions demandées sont converties ainsi :
    - `5 + 3` devient `((+) 5 3)`, qui donne 8.
    - `10 * 4` devient `((*) 10 4)`, qui donne 40.
    - `Vrai et Faux` (True and False) devient `((&&) True False)`, qui donne `False`.

- **Notation Infixe** :
  - Les fonctions données, comme `(+)` et `(*)`, sont des opérateurs qui, lorsqu'ils sont utilisés sans parenthèses, adoptent la notation infixe standard.
  - Les expressions demandées sont écrites ainsi :
    - `(+) 7 2` devient `7 + 2`, qui donne 9.
    - `(*) 6 5` devient `6 * 5`, qui donne 30.
    - `(&&) True False` devient `True && False`, qui donne `False`.

- **Fonction `main`** :
  - La fonction `main` utilise `putStrLn` pour afficher les résultats.
  - `show` est utilisé pour convertir les résultats (entiers, booléens) en chaînes pour l'affichage.

### Résultat Attendu
En exécutant ce programme (par exemple avec GHC), la sortie sera :

```
5 + 3 en notation préfixe : 8
10 * 4 en notation préfixe : 40
Vrai et Faux en notation préfixe : False
(+) 7 2 en notation infixe : 9
(*) 6 5 en notation infixe : 30
(&&) Vrai Faux en notation infixe : False
```

### Observations
- **Notation Préfixe vs Infixe** : En Haskell, les opérateurs peuvent être utilisés dans les deux formes. La notation préfixe est utile pour traiter les opérateurs comme des fonctions (par exemple, dans des fonctions d'ordre supérieur), tandis que la notation infixe est plus intuitive pour les calculs courants.
- **Types** :
  - `(+)` et `(*)` travaillent sur des nombres (par exemple, `Int` ou `Num a => a`).
  - `&&` est une opération booléenne qui retourne `False` si l'un des opérandes est `False`.
- **Immutabilité** : Bien que la demande ne mentionne pas explicitement l'immutabilité, les expressions sont évaluées de manière immuable, conformément au paradigme fonctionnel de Haskell.

Ce code illustre clairement la différence entre les notations préfixe et infixe tout en respectant les exigences de la requête.
