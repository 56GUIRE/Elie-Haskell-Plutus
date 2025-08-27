HC5T6 : Composition de fonctions
Voici le code Haskell qui utilise la composition de fonctions (.) pour prendre une liste de nombres, calculer leurs carrés et filtrer les nombres pairs, avec une fonction main :

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5, 6]
    print $ evenSquares numbers

evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)
```

Explications :
- La fonction `evenSquares` utilise la composition (.) pour combiner deux fonctions :
  - `map (^2)` élève chaque élément de la liste au carré.
  - `filter even` ne garde que les nombres pairs du résultat.
- La composition `filter even . map (^2)` applique d'abord `map (^2)`, puis `filter even` sur le résultat.
- Dans `main`, on définit une liste de test `[1, 2, 3, 4, 5, 6]` et on affiche le résultat de `evenSquares` appliqué à cette liste.
- Résultat pour `[1, 2, 3, 4, 5, 6]` : `[4, 16, 36]` (carrés de 2, 4, 6, qui sont pairs).

Le programme est concis et utilise l'opérateur de composition comme demandé.
