{-# LANGUAGE OverloadedStrings #-}

{-
    Deploy the book to GitHub.

    $ stack ghc -- Deploy.hs
    $ ./Deploy "Commit message"

    or

    $ stack exec runhaskell Deploy.hs "Commit message"
-}

module Main where

import           Shelly
import qualified Data.Text              as T
import           Control.Monad          (void)
import           System.Environment     (getArgs)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception.Base

main :: IO ()
main = void . shelly $ do
    args <- liftIO getArgs
    if length args /= 1 then commitMessagePlease else do
        let [commitMessageRaw] = args
            commitMessage      = T.pack commitMessageRaw

        echo "Собираем новую версию книги..."
        run "ohaskell" []

        echo "Учитываем изменения в ветке 'master'..."
        gitAdd ["."]
        gitCommit [commitMessage]
        gitPush ["master"]

        echo "Копируем во временное место, предварительно удалив старое, если нужно..."
        rm_rf "/tmp/_site" `catch_sh` ifNot
        cp_r "_site" "/tmp"

        echo "Переключаемся на ветку 'gh-pages'..."
        gitCheckout ["gh-pages"]

        echo "Удаляем ненужное..."
        rm_f  "*.html"
        rm_rf "static"
        rm_f  "*.md"
        rm_f  "*.cabal"
        rm_f  "*.hs"

        echo "Копируем..."
        cp_r "/tmp/_site/." "."

        rm_rf "chapters"
        rm_rf "src"
        rm_rf "templates"
        rm_rf "epub"
        rm_rf "pdf"
        rm_rf "_site"
        rm_rf "_cache"
        rm_f  "*.md"
        rm_f  "*.cabal"
        rm_f  "*.hs"
        rm_f  "*.config"
        rm_f  "Deploy"

        echo "Учитываем все изменения и публикуем на GitHub Pages..."
        gitAdd ["."]
        gitCommit [commitMessage] `catch_sh` ifNot
        gitPush ["gh-pages"] `catch_sh` ifNot

        echo "Возвращаемся в ветку 'master'..."
        gitCheckout ["master"]

        echo "Готово!"
  where
    commitMessagePlease = liftIO . putStrLn $
        "Сообщение о коммите забыли, нужно ./Deploy \"Что-нибудь интересное.\""

    gitAdd      = command_ "git" ["add"]
    gitCommit   = command_ "git" ["commit", "-a", "-m"]
    gitPush     = command_ "git" ["push", "origin"]
    gitCheckout = command_ "git" ["checkout"]

    ifNot :: SomeException -> Sh ()
    ifNot _ = return ()

