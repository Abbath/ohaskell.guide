{-# LANGUAGE OverloadedStrings #-}

module SingleMarkdown (
      createSingleMarkdown
    , ChapterName
    , ChapterPath
    , ChapterPoint
) where

import           Data.Maybe             (fromMaybe)
import           Data.List              (sort)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Directory       (getDirectoryContents)
import           System.FilePath.Posix  (takeExtensions, replaceExtension)

type ChapterName     = T.Text
type ChapterPath     = FilePath
type ChapterContent  = T.Text
type ChaptersContent = T.Text
type ChapterPoint    = (ChapterName, ChapterPath)

createSingleMarkdown :: IO (FilePath, [ChapterPoint], [ChapterPoint])
createSingleMarkdown = do
    chaptersInfo <- collectMarkdownInfoFrom ""          "chapters" 3
    practiceInfo <- collectMarkdownInfoFrom "/practice" "practice" 4
    let singleMarkdown = composeSingleMarkdownFrom chaptersInfo
        chapterPoints  = collectChapterPointsFrom chaptersInfo
        practicePoints = collectChapterPointsFrom practiceInfo
    TIO.writeFile pathToSingleMarkdown singleMarkdown
    return (pathToSingleMarkdown, chapterPoints, practicePoints)
  where
    collectMarkdownInfoFrom :: String
                            -> String
                            -> Int
                            -> IO [(ChapterContent, ChapterName, ChapterPath)]
    collectMarkdownInfoFrom root directory numberPrefix = do
        allPathsToMarkdownFiles <- getDirectoryContents directory
        let pathsToMarkdownFiles = filter markdownOnly $ sort allPathsToMarkdownFiles
        mapM (readMarkdownFile root directory numberPrefix) pathsToMarkdownFiles

    pathToSingleMarkdown = "/tmp/ohaskell-book.md"

    markdownOnly path = takeExtensions path == ".md"

readMarkdownFile :: String
                 -> String
                 -> Int
                 -> ChapterPath
                 -> IO (ChapterContent, ChapterName, ChapterPath)
readMarkdownFile root directory numberPrefix path = do
    content <- TIO.readFile $ directory ++ "/" ++ path
    let name = extractChapterNameFrom content
    return (content, name, htmlPath)
  where
    -- Убираем префиксные номера из глав, на уровне путей они не нужны.
    htmlPath = root ++ "/" ++ drop numberPrefix (replaceExtension path "html")
    extractChapterNameFrom aContent = aName
      where
        firstLine = head . T.lines $ aContent
        aName     = T.strip $ fromMaybe firstLine (T.stripPrefix "#" firstLine)

composeSingleMarkdownFrom :: [(ChapterContent, ChapterName, ChapterPath)] -> ChaptersContent
composeSingleMarkdownFrom chaptersInfo =
    T.intercalate "\n" [content | (content, _, _) <- chaptersInfo]

collectChapterPointsFrom :: [(ChapterContent, ChapterName, ChapterPath)] -> [ChapterPoint]
collectChapterPointsFrom chaptersInfo =
    [(name, path) | (_, name, path) <- chaptersInfo]

