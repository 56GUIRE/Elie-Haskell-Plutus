Voici une solution en Haskell qui répond à votre demande. Nous allons définir un type `Employee` en syntaxe d'enregistrement avec les champs `name :: String` et `experienceInYears :: Float`, créer un employé nommé "Richard" avec 7.5 ans d'expérience, et inclure une fonction `main` pour tester.

### Code Haskell :
```haskell
-- Définir le type Employee en syntaxe d'enregistrement
data Employee = Employee {
  name :: String,
  experienceInYears :: Float
} deriving (Show)

-- Créer un employé Richard
richard :: Employee
richard = Employee {
  name = "Richard",
  experienceInYears = 7.5
}

-- Fonction main pour tester
main :: IO ()
main = do
  print richard
```

### Explications :
1. **Type `Employee`** :
   - `data Employee = Employee { name :: String, experienceInYears :: Float }` définit un type d'enregistrement avec deux champs :
     - `name :: String` pour le nom de l'employé.
     - `experienceInYears :: Float` pour le nombre d'années d'expérience (en utilisant `Float` pour permettre les décimales).
   - `deriving (Show)` permet d'afficher les instances de `Employee` de manière lisible.

2. **Instance `richard`** :
   - `richard` est une valeur de type `Employee` avec `name = "Richard"` et `experienceInYears = 7.5`.

3. **Fonction `main`** :
   - Utilise `print` pour afficher les détails de `richard`. Grâce à `deriving (Show)`, la sortie sera formatée automatiquement.

### Résultat attendu :
Lorsque vous exécutez ce programme, il affichera quelque chose comme :
```
Employee {name = "Richard", experienceInYears = 7.5}
```
(La sortie exacte peut varier légèrement selon l'implémentation de `Show`, mais elle montrera clairement les valeurs de `richard`.)

### Notes :
- Le type `Float` est utilisé pour `experienceInYears` afin de supporter des valeurs comme 7.5. Si vous préférez une précision plus élevée, vous pourriez utiliser `Double`.
- Ce code est simple et fonctionnel. Vous pouvez ajouter d'autres employés ou fonctions si nécessaire !

Testez ce code, et il devrait fonctionner comme attendu !
