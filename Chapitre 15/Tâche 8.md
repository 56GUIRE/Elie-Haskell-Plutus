Super, je suis content que ça marche ! Voici le code corrigé avec une explication détaillée pour vous aider à comprendre chaque partie.

### Code
```haskell
import Text.Read (readMaybe)

main :: IO ()
main = do
    let testCases = [("100", "10"), ("50", "0"), ("-20", "5"), ("abc", "15")] -- (dividende, diviseur)
    mapM_ processDivision testCases
  where
    processDivision (numStr, denomStr) = do
        let result = divideNumbers numStr denomStr
        case result of
            Left errMsg  -> putStrLn errMsg
            Right value -> putStrLn $ "Résultat = " ++ show value
    divideNumbers numStr denomStr = 
        let num = readMaybe numStr :: Maybe Double
            denom = readMaybe denomStr :: Maybe Double
        in case (num, denom) of
            (Just n, Just d) ->
                if d == 0
                    then Left $ "Erreur : Division par zéro avec " ++ numStr ++ " / " ++ denomStr
                    else Right (n / d)
            _ -> Left $ "Erreur : " ++ numStr ++ " ou " ++ denomStr ++ " n'est pas un nombre valide"
```

### Explication
1. **Importation** :
   - `import Text.Read (readMaybe)` : Importe la fonction `readMaybe` du module `Text.Read`. Cette fonction tente de convertir une chaîne en un type donné (ici `Double`) et renvoie `Maybe Double` (`Just valeur` si réussi, `Nothing` si échoué), permettant une gestion sécurisée des entrées.

2. **Fonction `main`** :
   - `let testCases = [("100", "10"), ("50", "0"), ("-20", "5"), ("abc", "15")]` : Définit une liste de paires `(dividende, diviseur)` à tester. Inclut des cas valides (ex. "100" / "10"), une division par zéro ("50" / "0"), et une entrée invalide ("abc" / "15").
   - `mapM_ processDivision testCases` : Utilise `mapM_` pour appliquer la fonction `processDivision` à chaque paire dans `testCases`. `mapM_` exécute chaque appel dans le contexte `IO` et affiche les résultats sans retourner de liste.

3. **Fonction `processDivision`** :
   - Prend une paire `(numStr, denomStr)` (chaînes représentant le dividende et le diviseur).
   - `let result = divideNumbers numStr denomStr` : Calcule le résultat en appelant `divideNumbers`, qui renvoie un `Either String Double`.
   - `case result of` :
     - `Left errMsg` : Si une erreur survient, affiche le message d'erreur avec `putStrLn errMsg`.
     - `Right value` : Si le calcul réussit, affiche "Résultat = " suivi de la valeur avec `putStrLn $ "Résultat = " ++ show value`.

4. **Fonction `divideNumbers`** :
   - `let num = readMaybe numStr :: Maybe Double` : Tente de convertir `numStr` en `Double`.
   - `let denom = readMaybe denomStr :: Maybe Double` : Tente de convertir `denomStr` en `Double`.
   - `in case (num, denom) of` : Évalue les deux résultats :
     - `(Just n, Just d)` : Si les deux conversions réussissent :
       - `if d == 0` : Vérifie si le diviseur est zéro et renvoie `Left` avec un message d'erreur spécifique (ex. "Erreur : Division par zéro avec 50 / 0").
       - Sinon, renvoie `Right (n / d)` avec le résultat de la division.
     - `_` : Si une conversion échoue (par ex. "abc"), renvoie `Left` avec un message d'erreur générique (ex. "Erreur : abc ou 15 n'est pas un nombre valide").

5. **Utilisation de `Either`** :
   - `Either String Double` est utilisé pour représenter soit une erreur (`Left` avec un message `String`) soit un succès (`Right` avec une valeur `Double`). Cela permet de fournir des messages d'erreur détaillés, répondant à votre demande.

### Résultat attendu
À 14:07 GMT le 18 octobre 2025, en exécutant ce code, vous devriez voir :
```
Résultat = 10.0
Erreur : Division par zéro avec 50 / 0
Résultat = -4.0
Erreur : abc ou 15 n'est pas un nombre valide
```

### Remarques
- Le code utilise des entrées simulées via `testCases` car l'environnement semble ne pas supporter `getLine`. Vous pouvez modifier `testCases` pour tester d'autres valeurs (par ex. `("75.5", "2.5")`).
- Les messages d'erreur sont détaillés grâce à `Either`, comme demandé.
- Si vous avez besoin d'interaction en temps réel à l'avenir, cela nécessiterait un environnement prenant en charge `getLine`.

Testez à nouveau si besoin, et dites-moi si vous voulez ajouter des fonctionnalités !
