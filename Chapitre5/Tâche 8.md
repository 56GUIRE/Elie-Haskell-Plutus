HC5T8 : Style sans point
En Haskell, le style "sans point" (point-free) consiste à exprimer une fonction sans mentionner explicitement ses arguments, en utilisant la composition de fonctions ou d'autres opérateurs. La fonction donnée `addFive x = x + 5` peut être réécrite en style sans point en utilisant l'opérateur de section. Voici le code Haskell avec une fonction `main` :

```haskell
main :: IO ()
main = print $ addFive 10  -- Affiche 15

addFive :: Int -> Int
addFive = (+5)
```

### Explications :
- **Style sans point** : La fonction `addFive x = x + 5` est réécrite comme `addFive = (+5)`, où `(+5)` est une section de l'opérateur `+`. Cela signifie que l'argument est implicitement appliqué à l'opérateur `+` avec `5` comme second opérande. En style sans point, on omet l'argument `x`.
- **Type** : La signature `Int -> Int` indique que la fonction prend un entier et retourne un entier.
- **Main** : La fonction `main` teste `addFive` avec l'entrée `10`, ce qui donne `10 + 5 = 15`, affiché via `print`.
- **Résultat** : Pour l'entrée `10`, le programme affiche `15`.

Ce code est concis, respecte le style sans point, et inclut une fonction `main` comme demandé. Si vous avez besoin d'une variation ou d'une clarification supplémentaire, faites-le-moi savoir !
