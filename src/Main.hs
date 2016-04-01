module Main where

import SingleMarkdown
import CreatePdf
import CreateEpub
import CreateHtml
import CreateHtmlTemplates

import Control.Concurrent.Async
import System.Environment
import System.Exit

main :: IO ()
main = do
    (pathToSingleMarkdown, chapterPoints) <- createSingleMarkdown

    args <- getArgs
    check args

    buildEpubIfNecessary args pathToSingleMarkdown
    buildPdfIfNecessary  args pathToSingleMarkdown
    buildHtmlIfNecessary args chapterPoints

buildEpubIfNecessary :: [String] -> FilePath -> IO ()
buildEpubIfNecessary args pathToSingleMarkdown =
    if buildAllOrJust epub args then do
        buildingVersion epub
        createEpub pathToSingleMarkdown
    else
        return ()

buildPdfIfNecessary :: [String] -> FilePath -> IO ()
buildPdfIfNecessary args pathToSingleMarkdown =
    if buildAllOrJust pdf args then do
        -- Самые тяжеловесные операции.
        buildingVersion pdf
        pdfDesktopDone <- async $ createPdfDesktop pathToSingleMarkdown
        pdfMobileDone  <- async $ createPdfMobile pathToSingleMarkdown
        wait pdfDesktopDone
        wait pdfMobileDone
    else
        return ()

buildHtmlIfNecessary :: [String] -> [ChapterPoint] -> IO ()
buildHtmlIfNecessary args chapterPoints =
    if buildAllOrJust html args then do
        buildingVersion html
        createHtmlTemplates chapterPoints
        -- Аргумент rebuild нужен для Hakyll.
        withArgs ["rebuild"] $ createHtml chapterPoints
    else
        return ()

check :: [String] -> IO ()
check args =
    if someInvalidArgs then do
        putStrLn $ "Usage: ohaskell ["  ++ pdf ++
                                    "|" ++ epub ++
                                    "|" ++ html ++ "]"
        exitFailure
    else
        return ()
  where
    someInvalidArgs = not . null $ filter invalid args
    invalid arg     =    arg /= pdf
                      && arg /= epub
                      && arg /= html

buildAllOrJust :: String -> [String] -> Bool
buildAllOrJust some args = some `elem` args || null args

pdf :: String
pdf = "--pdf"

epub :: String
epub = "--epub"

html :: String
html = "--html"

buildingVersion :: String -> IO ()
buildingVersion ver = putStrLn $ " Build " ++ drop 2 ver ++ " version..."