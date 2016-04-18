# Весёлые репозитории

Исправьте ошибку:

```haskell
module Main where

ghRepoURLs :: [String]
ghRepoURLs = [ "https://github.com/denisshevchenko/ohaskell.guide"
             , "https://github.com/zalora/abacate"
             ]

main :: IO ()
main =
  let [bookRepoURL, _] = ghRepoURLs
       userAndRepo     = drop ghPrefix bookRepoURL
       ghPrefix        = 19
  in putStrLn userAndRepo
```

SOLUTION_BEGIN
Проблема в форматировании. Выражения, введённые с помощью `let` и не написанные в одну строку, должны быть строго выровнены по левому краю:

```haskell
      |
  let [bookRepoURL, _] = ghRepoURLs
       userAndRepo     = drop ghPrefix bookRepoURL
       ghPrefix        = 19
      |
```

Видите лишний отступ в один пробел перед `userAndRepo` и `ghPrefix`? Таким образом, правильный вариант такой:

```haskell
      |
  let [bookRepoURL, _] = ghRepoURLs
      userAndRepo      = drop ghPrefix bookRepoURL
      ghPrefix         = 19
      |
```
SOLUTION_END

