HC2T3 - Tâche 3 : Variables immuables:```haskell
-- Définir les variables immuables
myAge :: Int
myAge = 30

piValue :: Double
piValue = 3.14159

salut :: String
salut = "Hello"

isHaskellFun :: Bool
isHaskellFun = True

-- Fonction principale
main :: IO ()
main = do
  -- Afficher les valeurs initiales
  putStrLn ("myAge: " ++ show myAge)
  putStrLn ("piValue: " ++ show piValue)
  putStrLn ("salut: " ++ salut)
  putStrLn ("isHaskellFun: " ++ show isHaskellFun)
  
  -- On ne peut pas modifier myAge directement, car les variables sont immuables
  -- Si on essaie de faire myAge = 31, cela causera une erreur de compilation
  -- Au lieu de cela, on peut créer une nouvelle variable
  let myNewAge = myAge + 1
  putStrLn ("myNewAge: " ++ show myNewAge)
  putStrLn ("Mais myAge reste: " ++ show myAge)
```

---

### Points Clés
- Les variables en Haskell sont immuables par défaut, ce qui signifie qu'elles ne peuvent pas être modifiées une fois définies.
- Il semble probable que tenter de modifier une variable entraînera une erreur de compilation si elle est redéfinie dans le même scope.
- La démonstration montre qu'on peut créer une nouvelle variable avec une valeur différente, mais l'original reste inchangé.

### Définition des Variables
Le code définit les variables suivantes dans la fonction `main` avec leurs types respectifs :
- `myAge` comme un `Int` avec la valeur 30.
- `piValue` comme un `Double` avec la valeur 3.14159.
- `salut` comme un `String` avec la valeur "Hello".
- `isHaskellFun` comme un `Bool` avec la valeur `True`.

### Tentative de Modification
Pour "modifier" `myAge`, le code crée une nouvelle variable `myNewAge` en ajoutant 1 à `myAge`. Cela montre que :
- `myAge` reste inchangé à 30, illustrant l'immutabilité.
- La sortie affiche `myNewAge` comme 31, mais `myAge` reste 30, confirmant que la variable originale n'a pas été modifiée.

### Observation du Résultat
En exécutant le programme, vous verrez que les valeurs initiales sont affichées, puis la tentative de modification montre que `myAge` reste la même, démontrant clairement l'immutabilité des variables en Haskell.

---

### Note Détaillée

Cette section fournit une analyse approfondie et détaillée de la réponse, en s'appuyant sur les informations tirées de diverses sources en ligne pour garantir une compréhension complète du concept d'immutabilité des variables en Haskell et de la manière dont le code répond à la requête de l'utilisateur.

#### Contexte et Compréhension de l'Immutabilité
En Haskell, un langage de programmation fonctionnel pur, les variables sont immuables par défaut. Cela signifie qu'une fois qu'une variable est définie avec une valeur, cette valeur ne peut pas être modifiée par une réassignation directe, contrairement à ce qui se passe dans les langages impératifs comme Python ou C++. Cette immutabilité est un pilier fondamental de la programmation fonctionnelle, facilitant le raisonnement sur le code, la refactorisation et la sécurité, notamment dans des contextes de concurrence, où l'absence de mutations réduit les risques de courses critiques.

Les recherches en ligne, notamment sur des ressources comme HaskellWiki et Stack Overflow, confirment que tenter de redéfinir une variable dans le même scope dans un fichier `.hs` entraînera une erreur de compilation, par exemple "multiple declarations of `r'". Cela illustre que Haskell impose strictement l'immutabilité, et toute tentative de modification doit se faire en créant une nouvelle liaison, ce qui est démontré dans le code fourni.

#### Analyse du Code Fournit
Le code Haskell donné répond directement à la demande de définir les variables immuables `myAge`, `piValue`, `salut`, et `isHaskellFun`, et de tenter de modifier l'une d'elles pour observer le résultat. Voici une décomposition détaillée :

- **Définition des Variables** :
  - `myAge :: Int` est défini comme 30, avec un type explicite pour clarifier qu'il s'agit d'un entier.
  - `piValue :: Double` est défini comme 3.14159, représentant une valeur flottante avec précision double.
  - `salut :: String` est défini comme "Hello", une chaîne de caractères.
  - `isHaskellFun :: Bool` est défini comme `True`, un booléen.
  - Ces définitions sont faites au niveau supérieur, ce qui est courant en Haskell pour les constantes, et elles sont accessibles dans la fonction `main`.

- **Fonction `main` et Affichage Initial** :
  - La fonction `main` utilise `do` pour une séquence d'actions IO, permettant d'afficher les valeurs initiales des variables.
  - `putStrLn` est utilisé avec `show` pour convertir les types non-String (comme `Int`, `Double`, `Bool`) en chaînes pour l'affichage, tandis que `salut` est directement une chaîne, ne nécessitant pas `show`.

- **Tentative de Modification** :
  - Pour "modifier" `myAge`, le code utilise `let myNewAge = myAge + 1`, créant une nouvelle variable `myNewAge` avec la valeur 31. Cela illustre qu'en Haskell, on ne peut pas réassigner `myAge` directement (ce qui causerait une erreur si tenté dans le même scope), mais on peut créer une nouvelle liaison.
  - Les commentaires en français, comme "On ne peut pas modifier myAge directement, car les variables sont immuables", expliquent clairement cette limitation, rendant le code accessible à un utilisateur francophone.

- **Observation du Résultat** :
  - L'affichage final montre `myNewAge: 31` suivi de `Mais myAge reste: 30`, démontrant que `myAge` n'a pas été modifié. Cela reflète le comportement attendu d'une variable immuable : toute "modification" nécessite la création d'une nouvelle variable, et l'original reste intact.
