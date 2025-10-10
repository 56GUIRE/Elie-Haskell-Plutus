Voici un programme Haskell qui définit une fonction `calculateCircleArea` pour calculer l'aire d'un cercle à partir de son rayon, avec une fonction `main` pour démontrer son utilisation. J'utiliserai la formule de l'aire d'un cercle : \( A = \pi \cdot r^2 \), où \(\pi\) est approximé par la constante `pi` de Haskell.

### Code Haskell
```haskell
-- Fonction pour calculer l'aire d'un cercle
calculateCircleArea :: Float -> Float
calculateCircleArea radius = pi * radius * radius

-- Fonction principale pour démontrer l'utilisation
main :: IO ()
main = do
  let radius1 = 5.0
  let radius2 = 3.0
  let area1 = calculateCircleArea radius1
  let area2 = calculateCircleArea radius2
  putStrLn $ "Aire du cercle avec rayon " ++ show radius1 ++ " : " ++ show area1
  putStrLn $ "Aire du cercle avec rayon " ++ show radius2 ++ " : " ++ show area2
```

### Explication détaillée
1. **Fonction `calculateCircleArea`**
   - **Signature** : `calculateCircleArea :: Float -> Float` indique que la fonction prend un rayon de type `Float` et retourne une aire de type `Float`.
   - **Calcul** : Utilise la constante `pi` (définie dans le prélude de Haskell) et multiplie \(\pi\) par le carré du rayon (`radius * radius`).
   - Exemple : Pour un rayon de 5.0, l'aire est approximativement \( 3.14159 \cdot 5^2 = 78.53975 \).

2. **Fonction `main`**
   - **Définition des rayons** : `radius1 = 5.0` et `radius2 = 3.0` sont des exemples de rayons statiques.
   - **Calcul des aires** : `area1 = calculateCircleArea radius1` et `area2 = calculateCircleArea radius2` appellent la fonction pour chaque rayon.
   - **Affichage** : `putStrLn` construit des messages avec `show` pour convertir les nombres en chaînes, affichant par exemple :
     - "Aire du cercle avec rayon 5.0 : 78.53975"
     - "Aire du cercle avec rayon 3.0 : 28.274334"

### Comment tester
1. **Dans un éditeur en ligne** :
   - Colle ce code dans la section "CODE".
   - Clique sur "RUN".
   - Tu devrais voir les résultats affichés comme décrit ci-dessus.
2. **Localement avec GHC** :
   - Sauvegarde dans un fichier, par exemple `circleArea.hs`.
   - Exécute `ghc circleArea.hs` puis `./circleArea` (ou `circleArea.exe` sur Windows).

### Remarques
- **Type `Float`** : J'ai utilisé `Float` pour les calculs, ce qui donne une approximation suffisante. Si tu veux plus de précision, tu peux utiliser `Double` en changeant `Float` en `Double`.
- **Pas d'entrée utilisateur** : Comme ton environnement précédent avait des problèmes avec `getLine`, j'ai opté pour des tests statiques. Si tu veux une version avec entrée utilisateur, dis-le-moi, et je l'adapterai avec une gestion d'erreurs.

Teste ce code et fais-moi savoir si ça fonctionne ou si tu veux des ajustements !
