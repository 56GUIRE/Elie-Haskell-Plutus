Voici un code Haskell qui implémente une fonction récursive pour convertir un nombre en une liste de ses chiffres, avec une fonction `main` incluse. La fonction décompose le nombre en ses chiffres individuels en utilisant la division et le reste.

```haskell
-- Définition de la fonction pour convertir un nombre en liste de chiffres
digits :: Integer -> [Integer]
digits 0 = []                  -- Cas de base : 0 retourne une liste vide
digits n
    | n > 0 = digits (n `div` 10) ++ [n `mod` 10]  -- Cas récursif : décompose n
    | otherwise = error "Entrée invalide : le nombre doit être positif ou nul"

-- Fonction principale
main :: IO ()
main = do
    let number = 12345 :: Integer  -- Nombre fixe pour tester
    print (digits number)          -- Affiche la liste des chiffres
```

### Explications :
1. **Fonction `digits`** :
   - `digits :: Integer -> [Integer]` : Déclare que `digits` prend un entier (`Integer`) comme entrée et retourne une liste d'entiers (`[Integer]`).
   - `digits 0 = []` : Cas de base, un nombre 0 retourne une liste vide (bien que cela puisse être ajusté selon les besoins).
   - `digits n` : Cas récursif, où :
     - `n > 0 = digits (n `div` 10) ++ [n `mod` 10]` : Si \( n \) est positif :
       - `n `div` 10` divise \( n \) par 10 pour prendre les chiffres supérieurs (par exemple, 12345 devient 1234).
       - `n `mod` 10` extrait le dernier chiffre (par exemple, 12345 donne 5).
       - La récursion continue avec la division, et les chiffres sont concaténés à l'envers avec `++`.
       - Par exemple, pour 12345 : `digits 1234 ++ [5]`, puis `digits 123 ++ [4] ++ [5]`, etc., jusqu'à `[1,2,3,4,5]`.
     - `otherwise = error "Entrée invalide : le nombre doit être positif ou nul"` : Gère les nombres négatifs en levant une erreur.

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let number = 12345 :: Integer` : Définit un nombre fixe pour tester.
   - `print (digits number)` : Affiche la liste des chiffres. Pour 12345, le résultat est `[1,2,3,4,5]`.

### Remarques :
- Cette implémentation retourne les chiffres dans l'ordre croissant (de gauche à droite, comme dans le nombre original), mais comme elle construit la liste en ajoutant à droite avec `++`, elle est moins efficace pour de grands nombres. Une version avec accumulation (via une liste auxiliaire) serait plus performante.
- Pour tester avec un autre nombre, change `number` (par exemple, `let number = 987` pour obtenir `[9,8,7]`).
- Si tu veux gérer les nombres négatifs différemment ou ajouter une version interactive, fais-le-moi savoir !

Copie ce code dans ton éditeur et exécute-le. Si tu as des questions ou veux des ajustements, dis-le-moi !
