{-# LANGUAGE OverloadedStrings #-}

module SingleMarkdown (
      createSingleMarkdown
    , ChapterName
    , ChapterPath
    , ChapterPoint
) where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Directory       (getDirectoryContents)
import           System.FilePath.Posix
import           Data.List

type ChapterName     = T.Text
type ChapterPath     = FilePath
type ChapterContent  = T.Text
type ChaptersContent = T.Text
type ChapterPoint    = (ChapterName, ChapterPath)

createSingleMarkdown :: IO (FilePath, [ChapterPoint])
createSingleMarkdown = do
    allPathsToMarkdownFiles <- getDirectoryContents "chapters"
    let pathsToMarkdownFiles = filter markdownOnly (sort allPathsToMarkdownFiles)
    chaptersInfo <- mapM readMarkdownFile pathsToMarkdownFiles
    let singleMarkdown = composeSingleMarkdownFrom chaptersInfo
        chapterPoints  = collectChapterPointsFrom chaptersInfo
    TIO.writeFile pathToSingleMarkdown singleMarkdown
    return (pathToSingleMarkdown, chapterPoints)
  where
    pathToSingleMarkdown = "/tmp/ohaskell-book.md"
    markdownOnly path = takeExtensions path == ".md"

readMarkdownFile :: ChapterPath -> IO (ChapterContent, ChapterName, ChapterPath)
readMarkdownFile path = do
    content <- TIO.readFile $ "chapters/" ++ path
    let name = extractChapterNameFrom content
    return (content, name, htmlPath)
  where
    -- Убираем `01-` из глав, на уровне путей они не нужны.
    htmlPath = "/" ++ (drop 3 $ replaceExtension path "html")
    extractChapterNameFrom aContent = aName
      where
        firstLine = head . T.lines $ aContent
        aName     = T.strip $ case T.stripPrefix "#" firstLine of
                                  Nothing      -> firstLine
                                  Just rawName -> rawName

composeSingleMarkdownFrom :: [(ChapterContent, ChapterName, ChapterPath)] -> ChaptersContent
composeSingleMarkdownFrom chaptersInfo =
    T.intercalate "\n" [content | (content, _, _) <- chaptersInfo]

collectChapterPointsFrom :: [(ChapterContent, ChapterName, ChapterPath)] -> [ChapterPoint]
collectChapterPointsFrom chaptersInfo =
    [(name, path) | (_, name, path) <- chaptersInfo]

