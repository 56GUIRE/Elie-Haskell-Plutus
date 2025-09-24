Voici le code Haskell pour définir un type de données récursif `Tweet` représentant un tweet avec du contenu (une chaîne de caractères), un nombre de likes (un entier), et une liste de commentaires (eux-mêmes des `Tweet`). J'inclus également une fonction `main` pour tester ce type.

```haskell
-- Définition du type de données récursif Tweet
data Tweet = Tweet
  { content :: String        -- Contenu du tweet
  , likes :: Int            -- Nombre de likes
  , comments :: [Tweet]     -- Liste de commentaires (eux-mêmes des tweets)
  } deriving (Show)

-- Fonction main pour tester
main :: IO ()
main = do
  -- Création d'un tweet avec des commentaires
  let comment1 = Tweet "Great post!" 5 []
  let comment2 = Tweet "Interesting!" 3 []
  let mainTweet = Tweet "Hello, world!" 10 [comment1, comment2]
  
  -- Affichage du tweet
  print mainTweet
```

### Explications :
1. **Type `Tweet`** :
   - `Tweet` est un type de données récursif avec trois champs :
     - `content :: String` : le texte du tweet.
     - `likes :: Int` : le nombre de likes.
     - `comments :: [Tweet]` : une liste de commentaires, où chaque commentaire est lui-même un `Tweet`, ce qui rend le type récursif.
   - Le `deriving (Show)` permet d'afficher les valeurs de `Tweet` de manière lisible.
2. **Fonction `main`** :
   - Crée deux commentaires (`comment1` et `comment2`) avec du contenu, des likes, et une liste de commentaires vide.
   - Crée un tweet principal (`mainTweet`) avec le contenu `"Hello, world!"`, 10 likes, et une liste contenant les deux commentaires.
   - Utilise `print` pour afficher la structure du tweet.

### Résultat attendu :
En exécutant le programme, la sortie sera (formatée pour plus de clarté ici) :
```
Tweet {content = "Hello, world!", likes = 10, comments = [Tweet {content = "Great post!", likes = 5, comments = []}, Tweet {content = "Interesting!", likes = 3, comments = []}]}
```

Ce code définit correctement un type récursif `Tweet` et montre comment l'utiliser. Si tu veux ajouter des fonctionnalités (comme compter le nombre total de likes ou afficher les tweets différemment), n'hésite pas à me le demander !
