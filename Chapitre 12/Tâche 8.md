Voici un programme Haskell qui définit une fonction `mergeLists` pour fusionner deux listes triées en une seule liste triée, avec une fonction `main` pour tester cette fonctionnalité. La fusion sera effectuée en comparant les éléments des deux listes et en les combinant dans l'ordre croissant.

### Code Haskell
```haskell
-- Fonction pour fusionner deux listes triées
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- Fonction principale pour tester la fonctionnalité
main :: IO ()
main = do
  let list1 = [1, 3, 5]
  let list2 = [2, 4, 6]
  let mergedList = mergeLists list1 list2
  putStrLn "Première liste :"
  print list1
  putStrLn "Deuxième liste :"
  print list2
  putStrLn "Liste fusionnée :"
  print mergedList
```

### Explication détaillée
1. **Fonction `mergeLists`**
   - **Signature** : `mergeLists :: Ord a => [a] -> [a] -> [a]` indique que la fonction prend deux listes de type `[a]` (où `a` doit appartenir à la classe `Ord` pour permettre les comparaisons) et retourne une liste `[a]`.
   - **Cas de base** :
     - `mergeLists xs [] = xs` : Si la deuxième liste est vide, retourne la première liste.
     - `mergeLists [] ys = ys` : Si la première liste est vide, retourne la deuxième liste.
   - **Cas récursif** :
     - `mergeLists (x:xs) (y:ys)` décompose les listes en leur tête (`x` ou `y`) et leur queue (`xs` ou `ys`).
     - `| x <= y = x : mergeLists xs (y:ys)` : Si `x` est inférieur ou égal à `y`, ajoute `x` à la liste résultat et continue avec le reste de `xs` et `y:ys`.
     - `| otherwise = y : mergeLists (x:xs) ys` : Sinon, ajoute `y` et continue avec `x:xs` et le reste de `ys`.
   - Exemple : Pour `[1, 3, 5]` et `[2, 4, 6]`, la fusion donne `[1, 2, 3, 4, 5, 6]`.

2. **Fonction `main`**
   - **Définition des listes** : `list1 = [1, 3, 5]` et `list2 = [2, 4, 6]` sont des listes triées statiques pour le test.
   - **Fusion** : `mergedList = mergeLists list1 list2` appelle la fonction pour fusionner les listes.
   - **Affichage** : `putStrLn` et `print` affichent les listes originale et la liste fusionnée, par exemple :
     ```
     Première liste :
     [1,3,5]
     Deuxième liste :
     [2,4,6]
     Liste fusionnée :
     [1,2,3,4,5,6]
     ```

### Comment tester
1. **Dans un éditeur en ligne** :
   - Colle ce code dans la section "CODE".
   - Clique sur "RUN".
   - Tu devrais voir les résultats comme ci-dessus.
2. **Localement avec GHC** :
   - Sauvegarde dans un fichier, par exemple `mergeLists.hs`.
   - Exécute `ghc mergeLists.hs` puis `./mergeLists` (ou `mergeLists.exe` sur Windows).

### Remarques
- **Précondition** : La fonction suppose que les listes d'entrée sont déjà triées. Si elles ne le sont pas, le résultat ne sera pas forcément trié.
- **Pas d'entrée utilisateur** : Comme ton environnement précédent avait des problèmes avec `getLine`, j'ai utilisé des listes statiques. Si tu veux une version avec saisie utilisateur, dis-le-moi, et je l'adapterai avec une gestion d'erreurs.
- **Type polymorphe** : Grâce à `Ord a`, la fonction fonctionne avec n'importe quel type comparable (par exemple, `Int`, `Float`, ou même des chaînes).

Teste ce code et fais-moi savoir si ça fonctionne ou si tu veux des modifications !
