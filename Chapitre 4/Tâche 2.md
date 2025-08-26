dayType :: String -> String
dayType day = case day of
    "Saturday" -> "C'est le week-end !"
    "Sunday"   -> "C'est le week-end !"
    "Monday"   -> "C'est un jour de semaine."
    "Tuesday"  -> "C'est un jour de semaine."
    "Wednesday" -> "C'est un jour de semaine."
    "Thursday" -> "C'est un jour de semaine."
    "Friday"   -> "C'est un jour de semaine."
    _          -> "Jour invalide"

-- Tests
main :: IO ()
main = do
    putStrLn $ "dayType \"Saturday\": " ++ dayType "Saturday"    -- Affiche "dayType \"Saturday\": C'est le week-end !"
    putStrLn $ "dayType \"Monday\": " ++ dayType "Monday"        -- Affiche "dayType \"Monday\": C'est un jour de semaine."
    putStrLn $ "dayType \"Friday\": " ++ dayType "Friday"        -- Affiche "dayType \"Friday\": C'est un jour de semaine."
    putStrLn $ "dayType \"Invalid\": " ++ dayType "Invalid"      -- Affiche "dayType \"Invalid\": Jour invalide"
