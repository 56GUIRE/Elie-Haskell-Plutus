HC11T2 : Fonction fancyFunction pour WeAccept

```haskell
-- Définition de la classe WeAccept
class WeAccept a where
    fancyFunction :: a -> String

-- Data types exemples
data Cardano = Cardano deriving Show
data Cash = Cash deriving Show
data Country = France | USA | Japan deriving Show

-- Instances pour Cardano
instance WeAccept Cardano where
    fancyFunction _ = "Cardano accepte les paiements en ADA sur la blockchain !"

-- Instances pour Cash
instance WeAccept Cash where
    fancyFunction _ = "Cash accepte les paiements en espèces immédiats."

-- Instance pour Country (exemple avec des pays)
instance WeAccept Country where
    fancyFunction France = "Country France accepte l'Euro comme devise officielle."
    fancyFunction USA = "Country USA accepte le Dollar Américain."
    fancyFunction Japan = "Country Japan accepte le Yen comme monnaie."

-- Fonction testeur
tester :: IO ()
tester = do
    putStrLn "Test avec Cardano:"
    putStrLn $ fancyFunction Cardano
    putStrLn ""
    
    putStrLn "Test avec Cash:"
    putStrLn $ fancyFunction Cash
    putStrLn ""
    
    putStrLn "Test avec Country (France):"
    putStrLn $ fancyFunction France
    putStrLn ""
    
    putStrLn "Test avec Country (USA):"
    putStrLn $ fancyFunction USA

-- Main
main :: IO ()
main = tester
```

### Explications
- **Classe `WeAccept`** : Définit la méthode `fancyFunction` qui prend une valeur de type `a` et retourne une `String`.
- **Data types** : `Cardano` et `Cash` sont des types unitaires (sans valeurs internes). `Country` est un type énuméré avec des constructeurs pour des pays.
- **Instances** : Chaque type implémente `fancyFunction` de manière spécifique.
- **Testeur** : La fonction `tester` appelle `fancyFunction` sur des exemples et affiche les résultats. Elle est appelée dans `main`.
- **Compilation et exécution** : Compilez avec `ghc -o programme Main.hs` (si le fichier s'appelle `Main.hs`), puis exécutez `./programme`. Cela affichera les messages personnalisés.

