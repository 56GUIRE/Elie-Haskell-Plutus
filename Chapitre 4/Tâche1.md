
weatherReport :: String -> String
weatherReport condition = case condition of
    "sunny"  -> "Il fait beau et ensoleillé !"
    "rainy"  -> "N'oublie pas ton parapluie !"
    "cloudy" -> "Un peu gris, mais pas de pluie pour l'instant !"
    _        -> "Météo inconnue"

-- Tests
main :: IO ()
main = do
    putStrLn $ "weatherReport \"sunny\": " ++ weatherReport "sunny"    -- Affiche "weatherReport \"sunny\": Il fait beau et ensoleillé !"
    putStrLn $ "weatherReport \"rainy\": " ++ weatherReport "rainy"    -- Affiche "weatherReport \"rainy\": N'oublie pas ton parapluie !"
    putStrLn $ "weatherReport \"cloudy\": " ++ weatherReport "cloudy"  -- Affiche "weatherReport \"cloudy\": Un peu gris, mais pas de pluie pour l'instant !"
    putStrLn $ "weatherReport \"snowy\": " ++ weatherReport "snowy"    -- Affiche "weatherReport \"snowy\": Météo inconnue"
