HC11T8 : Dérivation de Eq et Ord pour PaymentMethod

```haskell
-- Définition du type PaymentMethod
data PaymentMethod
  = Cash
  | CreditCard String  -- Numéro de carte (simplifié)
  | PayPal String      -- Adresse email associée
  | BankTransfer String -- Numéro de compte bancaire
  deriving (Show, Eq, Ord)

-- Fonction main pour tester les comparaisons
main :: IO ()
main = do
  -- Définition de quelques méthodes de paiement
  let cash = Cash
  let card1 = CreditCard "1234-5678"
  let card2 = CreditCard "1234-5678"
  let paypal = PayPal "user@example.com"
  let bank = BankTransfer "IBAN123"

  putStrLn "Méthodes de paiement définies :"
  putStrLn $ "cash = " ++ show cash
  putStrLn $ "card1 = " ++ show card1
  putStrLn $ "card2 = " ++ show card2
  putStrLn $ "paypal = " ++ show paypal
  putStrLn $ "bank = " ++ show bank

  putStrLn "\nTests d'égalité (Eq) :"
  putStrLn $ "cash == card1 = " ++ show (cash == card1) -- False
  putStrLn $ "card1 == card2 = " ++ show (card1 == card2) -- True
  putStrLn $ "paypal == bank = " ++ show (paypal == bank) -- False

  putStrLn "\nTests d'ordre (Ord) :"
  putStrLn $ "cash < card1 = " ++ show (cash < card1) -- True (Cash est avant CreditCard)
  putStrLn $ "card1 < paypal = " ++ show (card1 < paypal) -- True (CreditCard est avant PayPal)
  putStrLn $ "paypal > bank = " ++ show (paypal > bank) -- True (PayPal est avant BankTransfer)
  putStrLn $ "card1 `compare` card2 = " ++ show (card1 `compare` card2) -- EQ
  putStrLn $ "cash `compare` bank = " ++ show (cash `compare` bank) -- LT
```

### Explications :
1. **Type `PaymentMethod`** :
   - Déclaré comme un type de données énuméré avec quatre constructeurs : `Cash`, `CreditCard`, `PayPal`, et `BankTransfer`.
   - `CreditCard`, `PayPal`, et `BankTransfer` incluent une chaîne de caractères pour représenter des informations supplémentaires (par exemple, numéro de carte, email, ou IBAN).
   - Les instances `Eq` et `Ord` sont dérivées automatiquement avec `deriving (Show, Eq, Ord)`.

2. **Instance `Eq` (dérivée)** :
   - Deux valeurs de `PaymentMethod` sont égales si elles utilisent le même constructeur et, pour les constructeurs avec des champs (comme `CreditCard`), si leurs champs sont égaux.
   - Exemple : `CreditCard "1234-5678" == CreditCard "1234-5678"` est `True`, mais `CreditCard "1234-5678" == CreditCard "9876-5432"` est `False`.

3. **Instance `Ord` (dérivée)** :
   - L'ordre est basé sur l'ordre de déclaration des constructeurs dans la définition du type :
     - `Cash` < `CreditCard` < `PayPal` < `BankTransfer`.
   - Pour les constructeurs avec des champs (comme `CreditCard`), si les constructeurs sont les mêmes, les champs sont comparés dans l'ordre (ici, les chaînes de caractères sont comparées lexicographiquement).
   - La fonction `compare` retourne `LT`, `EQ`, ou `GT` en fonction de cette logique.

4. **Fonction `main`** :
   - Crée plusieurs valeurs de `PaymentMethod` pour tester.
   - Affiche les méthodes de paiement pour référence.
   - Teste l'égalité avec `==` (via `Eq`).
   - Teste l'ordre avec `<`, `>`, et `compare` (via `Ord`).
   - Montre des comparaisons entre différents constructeurs et entre instances du même constructeur.
