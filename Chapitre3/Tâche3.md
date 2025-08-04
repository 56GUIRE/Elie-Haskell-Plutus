HC3T3 - Tâche 3 : Convertir une couleur RGB en chaîne hexadécimale avec let:

```haskell
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) = 
    let toHex n = 
            let hexDigits = "0123456789ABCDEF"
                clamped = max 0 (min 255 n) -- S'assurer que la valeur est entre 0 et 255
                first = hexDigits !! (clamped `div` 16)
                second = hexDigits !! (clamped `mod` 16)
            in [first, second]
        rHex = toHex r
        gHex = toHex g
        bHex = toHex b
    in rHex ++ gHex ++ bHex

main :: IO ()
main = do
    putStrLn $ "rgbToHex (255, 0, 127): " ++ rgbToHex (255, 0, 127)
    putStrLn $ "rgbToHex (0, 255, 64): " ++ rgbToHex (0, 255, 64)
```

### Explication :
- La fonction `rgbToHex` prend un tuple `(Int, Int, Int)` représentant les composants rouge, vert et bleu.
- Une fonction auxiliaire `toHex` est définie dans un `let` pour convertir un entier en une chaîne hexadécimale à deux caractères :
  - Elle utilise une liste `hexDigits` pour les chiffres hexadécimaux (0-9, A-F).
  - Elle borne la valeur d'entrée entre 0 et 255 avec `max 0 (min 255 n)`.
  - Elle calcule les deux chiffres hexadécimaux en divisant par 16 (pour le premier chiffre) et en prenant le reste modulo 16 (pour le second).
- Les composants RVB sont convertis en hexadécimal (`rHex`, `gHex`, `bHex`) et concatenés avec `++`.
- Le `main` teste la fonction avec `(255, 0, 127)` et `(0, 255, 64)`.

### Sortie attendue :
```
rgbToHex (255, 0, 127): FF007F
rgbToHex (0, 255, 64): 00FF40
```
