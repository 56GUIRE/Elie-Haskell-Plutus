Voici le code Haskell qui définit une fonction `parseShape` pour parser une chaîne (`String`) et retourner un `Shape`, avec un `main` pour tester. Le type `Shape` inclut des constructeurs comme `Circle` et `Rectangle`, et la fonction gère la conversion à partir d'une chaîne.

```haskell
module Main where

-- Définition du type de données Shape
data Shape = Circle Double | Rectangle Double Double
  deriving (Show)

-- Fonction pour parser une chaîne en Shape
parseShape :: String -> Shape
parseShape str =
  case words str of
    ["Circle", r] -> Circle (read r :: Double)
    ["Rectangle", w, h] -> Rectangle (read w :: Double) (read h :: Double)
    _ -> error "Format invalide : attendu 'Circle <rayon>' ou 'Rectangle <largeur> <hauteur>'"

-- Fonction main pour tester parseShape
main :: IO ()
main = do
  -- Tests avec différentes chaînes
  let circleStr = "Circle 5.0"
  let rectStr = "Rectangle 4.0 6.0"
  let invalidStr = "Invalid"
  
  putStrLn $ "Parsing '" ++ circleStr ++ "' : " ++ show (parseShape circleStr)
  putStrLn $ "Parsing '" ++ rectStr ++ "' : " ++ show (parseShape rectStr)
  -- Note : Le test avec une chaîne invalide lèvera une erreur, donc on le commente pour éviter un crash
  -- putStrLn $ "Parsing '" ++ invalidStr ++ "' : " ++ show (parseShape invalidStr)
```

### Explications du code :

1. **Module `Main`** :
   - `module Main where` déclare le module principal, point d'entrée du programme.

2. **Type de données `Shape`** :
   - `data Shape = Circle Double | Rectangle Double Double` définit un type avec deux constructeurs :
     - `Circle Double` pour un cercle avec un rayon.
     - `Rectangle Double Double` pour un rectangle avec une largeur et une hauteur.
   - `deriving (Show)` permet d'afficher les valeurs de `Shape` sous forme de chaînes.

3. **Fonction `parseShape`** :
   - **Signature** : `parseShape :: String -> Shape` prend une chaîne et retourne un `Shape`.
   - **Implémentation** :
     - `words str` divise la chaîne en une liste de mots (séparés par des espaces).
     - `case` analyse cette liste :
       - `["Circle", r]` : Si la liste contient `"Circle"` suivi d'un nombre `r`, crée un `Circle` en convertissant `r` en `Double` avec `read`.
       - `["Rectangle", w, h]` : Si la liste contient `"Rectangle"` suivi de deux nombres `w` et `h`, crée un `Rectangle` en convertissant `w` et `h` en `Double`.
       - `_ -> error "..."` : Pour tout autre format, lève une erreur avec un message explicite.
     - Les annotations `:: Double` forcent la conversion de `read` en type `Double`.

4. **Fonction `main`** :
   - **Type** : `main :: IO ()` effectue des opérations d'entrée/sortie.
   - **Structure `do`** : Exécute plusieurs actions d'affichage.
   - **Tests** :
     - `circleStr = "Circle 5.0"` teste le parsing d'un cercle.
     - `rectStr = "Rectangle 4.0 6.0"` teste le parsing d'un rectangle.
     - `invalidStr = "Invalid"` est commenté car `parseShape` lèverait une erreur pour une chaîne invalide, ce qui interromprait l'exécution.
   - `putStrLn` utilise `show` pour afficher les résultats sous forme de chaînes.

### Exécution :
- Compilez avec `ghc Main.hs` et exécutez `./Main`.
- Sortie attendue :
  ```
  Parsing 'Circle 5.0' : Circle 5.0
  Parsing 'Rectangle 4.0 6.0' : Rectangle 4.0 6.0
  ```
- Si tu décommente la ligne avec `invalidStr`, tu obtiendras une erreur comme `Format invalide : attendu 'Circle <rayon>' ou 'Rectangle <largeur> <hauteur>'`.

### Remarques :
- **Limitation** : La fonction suppose que les nombres dans la chaîne sont valides (`read` échouera si la conversion échoue). Pour une gestion d'erreur plus robuste, tu pourrais utiliser `reads` ou une monade comme `Maybe`.
- **Format attendu** : Les chaînes doivent être sous la forme `"Circle <nombre>"` ou `"Rectangle <nombre> <nombre>"`, avec des espaces comme séparateurs.
- **Sécurité** : L'utilisation de `error` est basique ; une version plus avancée pourrait retourner un `Maybe Shape` pour gérer les échecs sans crash.

Si tu veux améliorer la gestion des erreurs ou ajouter des tests, dis-le-moi !
