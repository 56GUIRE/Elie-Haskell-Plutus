HC15T7 : Calcul de vitesse avec gestion des valeurs optionnelles et du parsing
```haskell
import Text.Read (readMaybe)

main :: IO ()
main = do
    let testCases = [("100", "10"), ("50", "abc"), ("-20", "5"), ("", "15")] -- (distance, temps)
    mapM_ calculateSpeed testCases
  where
    calculateSpeed (distStr, timeStr) = do
        let distance = readMaybe distStr :: Maybe Double
        let time = readMaybe timeStr :: Maybe Double
        case (distance, time) of
            (Just d, Just t) ->
                if t == 0
                    then putStrLn $ "Erreur : Division par zéro pour " ++ distStr ++ "/" ++ timeStr
                    else putStrLn $ "Vitesse = " ++ show (d / t) ++ " unités par unité de temps"
            _ -> putStrLn $ "Erreur : " ++ distStr ++ " ou " ++ timeStr ++ " n'est pas un nombre valide"
```

### Explication
1. **Importation** :
   - `import Text.Read (readMaybe)` : Importe `readMaybe` pour convertir des chaînes en `Double` de manière sécurisée, renvoyant `Maybe Double`.

2. **Fonction `main`** :
   - `let testCases = [("100", "10"), ("50", "abc"), ("-20", "5"), ("", "15")]` : Définit une liste de paires (distance, temps) à tester. Inclut des cas valides, invalides, et un cas avec division par zéro.
   - `mapM_ calculateSpeed testCases` : Applique la fonction `calculateSpeed` à chaque paire et affiche les résultats.

3. **Fonction `calculateSpeed`** :
   - Prend une paire `(distStr, timeStr)` représentant les chaînes d'entrée pour la distance et le temps.
   - `readMaybe distStr :: Maybe Double` et `readMaybe timeStr :: Maybe Double` : Tente de parser les chaînes en nombres décimaux (`Double` pour permettre des valeurs non entières).
   - **Gestion des cas avec `case`** :
     - `(Just d, Just t)` : Si les deux conversions réussissent :
       - Vérifie si `t == 0` pour éviter la division par zéro, affiche une erreur si c'est le cas.
       - Sinon, calcule `d / t` et affiche la vitesse.
     - `_` : Si une conversion échoue (par exemple, "abc" ou ""), affiche un message d'erreur.

4. **Gestion sécurisée** :
   - `readMaybe` évite les crashs en cas d'entrée invalide (par exemple, lettres ou chaînes vides).
   - La vérification de `t == 0` empêche les erreurs de division par zéro.

