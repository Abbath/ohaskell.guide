{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-
    Деплоим книгу на GitHub (включая Pages).

    $ stack ghc -- Deploy.hs
    $ ./Deploy "Сообщение о коммите"

    или

    $ stack exec runhaskell Deploy.hs "Сообщение о коммите"
-}

module Main where

import Control.Monad            (unless)
import Data.List                (intercalate)
import System.Directory         ( createDirectory
                                , copyFile
                                , doesDirectoryExist
                                , removeDirectoryRecursive
                                )
import System.FilePath.Posix    ((</>))
import System.Process           (callProcess)
import System.Exit              (die)
import System.Environment       (getArgs)

main :: IO ()
main = do
    putStrLn $ "Собираем новую версию книги..."

    shouldBeInRepoRoot
    branchShouldBeMaster

    compileBook
    commitNPushToMasterIfNecessary
    rebuildBook

    -- Артефакты сборки (в ветке master не учитываются):
    -- _site                    -> полная веб-версия
    -- pdf/ohaskell.pdf         -> PDF A4
    -- pdf/ohaskell-mobile.pdf  -> PDF A5
    -- epub/ohaskell.epub       -> EPUB

    storeArtefactsInSite
    saveSiteInTempDirectory
    checkoutToGhPages
    cleanGhPages
    takeSiteFromTempDirectory
    commitNPushToGhPages
    backToMaster
    removeTempDirectory
  where
    shouldBeInRepoRoot = doesDirectoryExist ".git" >>= \inRepoRoot ->
        unless inRepoRoot $ die "Отсутствует .git-каталог, а он мне очень нужен!"

    branchShouldBeMaster = readFile ".git/HEAD" >>= \headValue ->
        unless (strip headValue == "ref: refs/heads/master") $
            die $ "Я желаю ветку master, а вовсе не '" ++ show headValue ++ "'..."
      where
        strip = intercalate "\n" . lines

    git_ = callProcess "git"

    commitNPushToMasterIfNecessary = do
        arguments <- getArgs
        if | null arguments -> do
               putStrLn "Сообщения о коммите нет, считаем, что в ветке master нет локальных изменений."
               return ()
           | length arguments == 1 -> do
               putStrLn "Учитываем изменения в ветке master..."
               let [commitMessage] = arguments
               git_ ["commit", "-a", "-m", commitMessage]
               git_ ["push", "origin", "master"]
           | otherwise -> die $
               "Запускайте с одним сообщением о коммите, или совсем без него."

    commitNPushToGhPages = do
        putStrLn "Учитываем изменения в ветке gh-pages..."
        git_ ["add", "."]
        git_ ["commit", "-a", "-m", "Current."]
        git_ ["push", "-f", "origin", "gh-pages"]

    compileBook = do
        putStrLn $ "Компилируем..."
        callProcess "stack" ["clean"]
        callProcess "stack" ["build"]

    rebuildBook = do
        putStrLn $ "Собираем..."
        callProcess "stack" ["exec", "--", "ohaskell"]

    fullWeb         = "_site"
    pdfBinary       = "ohaskell.pdf"
    pdfMobileBinary = "ohaskell-mobile.pdf"
    epubBinary      = "ohaskell.epub"
    storeArtefactsInSite = do
        createDirectory $ fullWeb </> "pdf"
        copyFile ("pdf" </> pdfBinary)       $ fullWeb </> "pdf" </> pdfBinary
        copyFile ("pdf" </> pdfMobileBinary) $ fullWeb </> "pdf" </> pdfMobileBinary

        createDirectory $ fullWeb </> "epub"
        copyFile ("epub" </> epubBinary)     $ fullWeb </> "epub" </> epubBinary

    saveSiteInTempDirectory     = callProcess "cp" ["-R", fullWeb, "/tmp"]
    checkoutToGhPages           = git_ ["checkout", "gh-pages"]
    resetLastCommit             = git_ ["reset", "--hard", "HEAD~1"]
    takeSiteFromTempDirectory   = callProcess "cp" ["-R", "/tmp" </> fullWeb ++ "/.", "."]
    removeTempDirectory         = removeDirectoryRecursive $ "/tmp" </> fullWeb
    backToMaster                = git_ ["checkout", "master"]

    cleanGhPages = do
        git_ ["add", "."]
        git_ ["commit", "-a", "-m", "Trash."]
        git_ ["reset", "--hard", "HEAD~2"]
