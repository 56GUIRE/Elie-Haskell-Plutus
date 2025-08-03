Voici le code en Haskell pour une fonction `greaterThan18` qui vérifie si un nombre est supérieur à 18, avec un `main` pour tester :

```haskell
-- Fonction pure pour vérifier si un nombre est supérieur à 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- Fonction main pour tester
main :: IO ()
main = do
    let number = 20
    print $ greaterThan18 number  -- Affiche True
    print $ greaterThan18 15      -- Affiche False
```

### Explications :
- `greaterThan18` : Une fonction pure qui prend un nombre (`x` de type `Int`) et retourne `True` si `x > 18`, sinon `False`.
- `main` : Teste la fonction avec deux valeurs : `20` (qui retourne `True`) et `15` (qui retourne `False`).
- La fonction est pure car elle ne dépend d'aucun état externe et donne toujours le même résultat pour la même entrée.

Vous pouvez exécuter ce code dans GHCi ou compiler un fichier `.hs`.
