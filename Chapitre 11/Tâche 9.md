 HC11T9 : Type Longueur avec unités

```haskell
-- Définition du type Length
data Length
  = Met Double  -- Mètres
  | Km Double   -- Kilomètres
  deriving (Show, Eq)

-- Instance Ord pour Length avec conversion mètres/kilomètres
instance Ord Length where
  compare l1 l2 = compare (toMeters l1) (toMeters l2)
    where
      toMeters :: Length -> Double
      toMeters (Met m) = m
      toMeters (Km k) = k * 1000  -- Convertit kilomètres en mètres

-- Fonction main pour tester
main :: IO ()
main = do
  -- Définition de quelques longueurs
  let len1 = Met 1000.0   -- 1000 mètres
  let len2 = Km 1.0       -- 1 kilomètre (1000 mètres)
  let len3 = Met 500.0    -- 500 mètres
  let len4 = Km 2.0       -- 2 kilomètres (2000 mètres)

  putStrLn "Longueurs définies :"
  putStrLn $ "len1 = " ++ show len1
  putStrLn $ "len2 = " ++ show len2
  putStrLn $ "len3 = " ++ show len3
  putStrLn $ "len4 = " ++ show len4

  putStrLn "\nTests d'égalité (Eq) :"
  putStrLn $ "len1 == len2 = " ++ show (len1 == len2) -- False (différents constructeurs)
  putStrLn $ "len1 == Met 1000.0 = " ++ show (len1 == Met 1000.0) -- True
  putStrLn $ "len2 == Km 1.0 = " ++ show (len2 == Km 1.0) -- True

  putStrLn "\nTests d'ordre (Ord) :"
  putStrLn $ "len1 `compare` len2 = " ++ show (len1 `compare` len2) -- EQ (1000 m == 1 km)
  putStrLn $ "len3 < len1 = " ++ show (len3 < len1) -- True (500 m < 1000 m)
  putStrLn $ "len4 > len1 = " ++ show (len4 > len1) -- True (2000 m > 1000 m)
  putStrLn $ "len3 < len4 = " ++ show (len3 < len4) -- True (500 m < 2000 m)
```

### Explications :
1. **Type `Length`** :
   - Déclaré avec deux constructeurs : `Met Double` pour les mètres et `Km Double` pour les kilomètres.
   - `deriving (Show, Eq)` permet d'afficher les valeurs et de dériver automatiquement l'instance `Eq`.

2. **Instance `Eq` (dérivée)** :
   - L'égalité est basée sur les constructeurs et leurs valeurs.
   - Par exemple, `Met 1000.0 == Met 1000.0` est `True`, mais `Met 1000.0 == Km 1.0` est `False` car les constructeurs sont différents, même si les valeurs sont équivalentes en mètres.

3. **Instance `Ord` (manuelle)** :
   - Définie manuellement pour gérer correctement les comparaisons entre mètres et kilomètres.
   - La fonction `toMeters` convertit toute longueur en mètres :
     - `Met m` retourne `m`.
     - `Km k` retourne `k * 1000`.
   - La méthode `compare` utilise `toMeters` pour comparer les longueurs en mètres, assurant une comparaison correcte (par exemple, `Met 1000.0` est égal à `Km 1.0` en termes d'ordre).

4. **Fonction `main`** :
   - Crée quatre longueurs : `Met 1000.0`, `Km 1.0`, `Met 500.0`, et `Km 2.0`.
   - Affiche les longueurs pour référence.
   - Teste l'égalité avec `==` (via `Eq` dérivée).
   - Teste l'ordre avec `compare`, `<`, et `>` (via `Ord` manuelle), en montrant que les comparaisons tiennent compte de la conversion mètres/kilomètres.

