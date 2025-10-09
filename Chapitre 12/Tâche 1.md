Voici un programme Haskell simple qui affiche « Bienvenue dans la programmation Haskell ! » dans le terminal, incluant une fonction `main` :

```haskell
-- Programme Haskell pour afficher un message de bienvenue
main :: IO ()
main = putStrLn "Bienvenue dans la programmation Haskell !"
```

### Explications :
- La fonction `main` est le point d'entrée du programme Haskell.
- `putStrLn` est une fonction de la bibliothèque standard qui affiche une chaîne de caractères dans le terminal, suivie d'un saut de ligne.
- Le message `"Bienvenue dans la programmation Haskell !"` est affiché directement lorsque le programme est exécuté.

### Comment exécuter :
1. Sauvegardez ce code dans un fichier, par exemple `Welcome.hs`.
2. Compilez-le avec GHC : `ghc Welcome.hs`.
3. Exécutez le programme généré : `./Welcome` (sur Linux/Mac) ou `Welcome.exe` (sur Windows).

### Sortie attendue :
```
Bienvenue dans la programmation Haskell !
```

Ce programme est minimal et répond exactement à la demande. Si vous avez besoin d'ajouter des fonctionnalités ou d'autres explications, faites-le-moi savoir !
