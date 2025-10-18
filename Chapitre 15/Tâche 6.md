HC15T6 : Analyse d'entrée utilisateur avec readMaybe:


```haskell
import Text.Read (readMaybe)

main = do
    let testCases = ["42", "-5", "0", "abc"] -- Différents cas à tester
    mapM_ processInput testCases
  where
    processInput input = case readMaybe input :: Maybe Int of
        Just n  -> if n > 0
                   then putStrLn (input ++ " est un nombre positif !")
                   else if n < 0
                        then putStrLn (input ++ " est un nombre négatif !")
                        else putStrLn (input ++ " est zéro !")
        Nothing -> putStrLn (input ++ " n'est pas un nombre entier valide.")
```

### Explication
1. **Importation** :
   - `import Text.Read (readMaybe)` : Importe la fonction `readMaybe` du module `Text.Read`. Cette fonction tente de convertir une chaîne en un type donné (ici `Int`) et renvoie `Just valeur` si ça réussit, ou `Nothing` si ça échoue, offrant ainsi une gestion sécurisée des entrées.

2. **Fonction `main`** :
   - `let testCases = ["42", "-5", "0", "abc"]` : Définit une liste de chaînes représentant des entrées simulées à tester (un nombre positif, un négatif, zéro, et une entrée invalide).
   - `mapM_ processInput testCases` : Utilise `mapM_` pour appliquer la fonction `processInput` à chaque élément de `testCases` et afficher les résultats. `mapM_` est une version de `map` qui fonctionne dans le contexte `IO` et ignore les valeurs de retour.

3. **Fonction `processInput`** :
   - Définie localement avec `where`, cette fonction prend une chaîne `input` et l'analyse avec `readMaybe input :: Maybe Int`.
   - **Cas `Just n`** : Si la conversion réussit (par exemple, "42" devient `Just 42`):
     - Vérifie si `n > 0` (positif), `n < 0` (négatif), ou `n == 0` (zéro), et affiche un message approprié avec la valeur originale (`input`) pour référence.
   - **Cas `Nothing`** : Si la conversion échoue (par exemple, "abc"), affiche un message d'erreur.

4. **Gestion sécurisée** :
   - Grâce à `readMaybe`, le programme ne plante pas avec des entrées invalides. Au lieu de cela, il passe au cas `Nothing` et informe l'utilisateur de l'erreur.

