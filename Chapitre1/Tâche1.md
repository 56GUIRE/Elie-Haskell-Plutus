Voici le code en Haskell pour les fonctions demandées :

```haskell
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double
```

Explications :
- `double` prend un nombre et le multiplie par 2
- `increment` prend un nombre et l'augmente de 1
- `doubleThenIncrement` utilise l'opérateur de composition `.` pour appliquer d'abord `double` puis `increment` au résultat

Exemple d'utilisation :
```haskell
main = print (doubleThenIncrement 5) -- Affiche 11 (car (5 * 2) + 1 = 11)
```
