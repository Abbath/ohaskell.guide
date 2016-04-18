{-# LANGUAGE OverloadedStrings #-}

module CreateEpub (
    createEpub
) where

import           System.Process     (callCommand)
import           System.Directory   (getHomeDirectory)

import           CreateEpubCss

createEpub :: FilePath -> IO ()
createEpub pathToSingleMarkdown = do
    home <- getHomeDirectory
    createEpubCss pathToCss
    callCommand $ concat [ "pandoc -S -o "
                         , out
                         , " --toc-depth=2"
                         , " --epub-stylesheet="
                         , pathToCss
                         , " --epub-embed-font="
                         , mainFont
                         , " --epub-embed-font="
                         , codeFontNormal home
                         , " --epub-embed-font="
                         , codeFontBold home
                         , " --epub-cover-image="
                         , cover
                         , " "
                         , title
                         , " "
                         , pathToSingleMarkdown
                         ]
  where
    out       = "epub/ohaskell.epub"
    pathToCss = "epub/EPUB.css"
    title     = "epub/EPUBTitle.txt"
    cover     = "epub/cover.png"

    -- Пути актуальны для OS X. Подразумевается,
    -- что данные шрифты уже установлены.
    mainFont = "/Library/Fonts/PTSerif.ttc"

    codeFontNormal :: FilePath -> FilePath
    codeFontNormal h = h ++ "/Library/Fonts/UbuntuMono-Regular.ttf"

    codeFontBold :: FilePath -> FilePath
    codeFontBold h = h ++ "/Library/Fonts/UbuntuMono-Bold.ttf"

