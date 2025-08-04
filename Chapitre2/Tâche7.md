HC2T7 - Tâche 7 : Expressions booléennes
### Code Haskell
```haskell
-- Fonction principale pour démontrer les expressions booléennes
main :: IO ()
main = do
  -- Expression avec && qui retourne True
  putStrLn $ "Vrai avec && : (True && True) = " ++ show (True && True)
  
  -- Expression avec || qui retourne False
  putStrLn $ "Faux avec || : (False || False) = " ++ show (False || False)
  
  -- Expression avec not qui retourne True
  putStrLn $ "Vrai avec not : (not False) = " ++ show (not False)
  
  -- Comparaison qui retourne False
  putStrLn $ "Comparaison fausse : (5 > 10) = " ++ show (5 > 10)
```

### Explications
1. **Expression avec `&&` retournant `True`** :
   - `(True && True)` : L’opérateur `&&` (ET logique) retourne `True` si les deux opérandes sont `True`. Ici, `True && True` donne `True`.

2. **Expression avec `||` retournant `False`** :
   - `(False || False)` : L’opérateur `||` (OU logique) retourne `False` si les deux opérandes sont `False`. Ici, `False || False` donne `False`.

3. **Expression avec `not` retournant `True`** :
   - `(not False)` : L’opérateur `not` (NON logique) inverse la valeur booléenne. Ici, `not False` donne `True`.

4. **Comparaison retournant `False`** :
   - `(5 > 10)` : Une comparaison simple avec l’opérateur `>` (plus grand que). Puisque 5 n’est pas supérieur à 10, cela retourne `False`.

5. **Fonction `main`** :
   - Utilise `putStrLn` pour afficher chaque expression et son résultat.
   - `show` convertit les valeurs booléennes (`True` ou `False`) en chaînes pour l’affichage.

### Résultat Attendu
En exécutant ce programme (par exemple avec GHC), la sortie sera :

```
Vrai avec && : (True && True) = True
Faux avec || : (False || False) = False
Vrai avec not : (not False) = True
Comparaison fausse : (5 > 10) = False
```

### Vérifications
- **Conformité** :
  - Chaque expression utilise l’opérateur spécifié (`&&`, `||`, `not`, ou une comparaison).
  - Les résultats sont `True` pour `&&` et `not`, `False` pour `||` et la comparaison, comme demandé.
- **Robustesse** : Les expressions sont simples et évaluées correctement, sans dépendances externes.
- **Clarté** : La sortie indique clairement chaque expression et son résultat.

### Note sur les Demandes Précédentes
Vous avez mentionné à plusieurs reprises une correction de code, mais cette demande semble indépendante des précédentes (qui concernaient `circleArea`, `maxOfThree`, variables immuables, notations préfixe/infixe, ou `2^64 :: Int`). Si vous souhaitiez inclure ces éléments ou si une erreur spécifique doit être corrigée, merci de préciser. Par exemple :
- Voulez-vous combiner ces expressions booléennes avec les fonctions ou variables des demandes précédentes ?
- Y a-t-il une erreur dans la syntaxe, le résultat, ou le format de la sortie ?
- Attendez-vous un format particulier pour la sortie (par exemple, plus ou moins de détails) ?

Ce code répond directement à votre dernière demande en fournissant des expressions booléennes avec les résultats spécifiés. Si vous avez des précisions ou une erreur à signaler, je peux ajuster davantage !
