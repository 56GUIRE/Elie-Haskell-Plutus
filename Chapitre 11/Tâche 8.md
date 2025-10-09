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
