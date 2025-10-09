HC11T6 : AdvancedEq pour Blockchain
```haskell
-- Définition d'un type pour représenter le résultat de la comparaison
data EqualityResult = Equal | NotEqual String deriving Show

-- Classe de type AdvancedEq qui étend Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> EqualityResult

-- Définition d'un type Block pour représenter un bloc dans la blockchain
data Block = Block
  { blockId :: Int
  , dataContent :: String
  } deriving (Show, Eq)

-- Définition du type Blockchain comme une liste de blocs
newtype Blockchain = Blockchain [Block] deriving Show

-- Instance Eq pour Blockchain (nécessaire pour AdvancedEq)
instance Eq Blockchain where
  (Blockchain blocks1) == (Blockchain blocks2) = blocks1 == blocks2

-- Instance AdvancedEq pour Blockchain
instance AdvancedEq Blockchain where
  compareEquality (Blockchain blocks1) (Blockchain blocks2)
    | blocks1 == blocks2 = Equal
    | length blocks1 /= length blocks2 =
        NotEqual $ "Blockchains have different lengths: " ++ show (length blocks1) ++ " vs " ++ show (length blocks2)
    | otherwise =
        NotEqual $ "Blockchains differ at some block: " ++ show (findFirstDiff blocks1 blocks2)
    where
      findFirstDiff :: [Block] -> [Block] -> Maybe (Block, Block)
      findFirstDiff [] [] = Nothing
      findFirstDiff (b1:bs1) (b2:bs2)
        | b1 /= b2 = Just (b1, b2)
        | otherwise = findFirstDiff bs1 bs2
      findFirstDiff _ _ = Nothing

-- Fonction main pour tester
main :: IO ()
main = do
  let block1 = Block 1 "Transaction1"
  let block2 = Block 2 "Transaction2"
  let block3 = Block 3 "Transaction3"
  let block4 = Block 3 "Transaction4" -- Différent contenu

  let chain1 = Blockchain [block1, block2, block3]
  let chain2 = Blockchain [block1, block2, block3]
  let chain3 = Blockchain [block1, block2, block4]

  putStrLn "Comparaison de chain1 et chain2 (identiques) :"
  putStrLn $ show (compareEquality chain1 chain2)

  putStrLn "\nComparaison de chain1 et chain3 (différentes) :"
  putStrLn $ show (compareEquality chain1 chain3)

  putStrLn "\nComparaison de chain1 et une chaîne plus courte :"
  let chain4 = Blockchain [block1]
  putStrLn $ show (compareEquality chain1 chain4)
```

### Explications :
1. **Type `EqualityResult`** :
   - Un type de données pour représenter le résultat de `compareEquality`.
   - `Equal` indique que les deux valeurs sont égales.
   - `NotEqual String` fournit une raison pour l'inégalité.

2. **Classe `AdvancedEq`** :
   - Définie comme une extension de `Eq` avec la contrainte `Eq a =>`.
   - Déclare la méthode `compareEquality :: a -> a -> EqualityResult`.

3. **Type `Block` et `Blockchain`** :
   - `Block` est une structure simple avec un `blockId` (entier) et un `dataContent` (chaîne).
   - `Blockchain` est un `newtype` enveloppant une liste de `Block`.
   - Une instance `Eq` est définie pour `Blockchain` en comparant les listes de blocs.

4. **Instance `AdvancedEq` pour `Blockchain`** :
   - La méthode `compareEquality` compare deux blockchains :
     - Si elles sont égales (via `==`), retourne `Equal`.
     - Si elles ont des longueurs différentes, retourne `NotEqual` avec un message indiquant la différence de longueur.
     - Si elles diffèrent sur un bloc, utilise une fonction auxiliaire `findFirstDiff` pour identifier le premier bloc différent et retourne un message détaillé.

5. **Fonction `main`** :
   - Crée des blocs et blockchains de test.
   - Teste `compareEquality` sur :
     - Deux blockchains identiques (`chain1` et `chain2`).
     - Deux blockchains différentes au niveau du contenu d'un bloc (`chain1` et `chain3`).
     - Une blockchain plus courte (`chain1` et `chain4`).
   - Affiche les résultats.

