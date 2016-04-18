{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module PrepareHtmlTOC (
    polishHtml
) where

import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Text.RawString.QQ

import           SingleMarkdown

polishHtml :: [ChapterPoint] -> [ChapterPoint] -> IO ()
polishHtml chapterPoints practicePoints = do
    prepareHtmlTOC ""          chapterPoints
    prepareHtmlTOC "/practice" practicePoints
    prepareHtmlPageTitles chapterPoints
    prepareHtmlPageTitles practicePoints
    prepareSolutionSectionsIn practicePoints

prepareHtmlTOC :: String -> [ChapterPoint] -> IO ()
prepareHtmlTOC root chapterPoints = V.mapM_ (handle root chaptersURLsWithIndex) chaptersURLsWithIndex
  where
    chaptersURLsWithIndex = V.indexed . V.fromList $ [path | (_, path) <- chapterPoints]

handle :: String -> V.Vector (Int, ChapterPath) -> (Int, ChapterPath) -> IO ()
handle root chaptersURLsWithIndex (currentChapterIndex, currentChapterURL)
    | currentChapterIndex == 0 = handleFirstChapter
    | otherwise = if currentChapterIndex + 1 == V.length chaptersURLsWithIndex
                      then handleLastChapter
                      else handleChapter
  where
    pathToCurrentChapter = "_site" ++ currentChapterURL
    initChapter = root ++ "/init.html" :: FilePath

    handleFirstChapter =
        let prevChapterURL      = initChapter
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleChapter =
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleLastChapter =
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            nextChapterURL      = initChapter
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    replaceInChapter prev next current = do
        chapter <- TIO.readFile current
        let chapter'  = T.replace "PREV_CHAPTER_URL" (T.pack prev) chapter
            chapter'' = T.replace "NEXT_CHAPTER_URL" (T.pack next) chapter'
        TIO.writeFile current chapter''

prepareHtmlPageTitles :: [ChapterPoint] -> IO ()
prepareHtmlPageTitles = mapM_ prepare
  where
    prepare :: (ChapterName, ChapterPath) -> IO ()
    prepare (name, rawPath) = do
        let path = "_site" ++ rawPath
        chapter <- TIO.readFile path
        let chapter' = T.replace "PAGE_TITLE" name chapter
        TIO.writeFile path chapter'

prepareSolutionSectionsIn :: [ChapterPoint] -> IO ()
prepareSolutionSectionsIn = mapM_ prepare
  where
    prepare :: (ChapterName, ChapterPath) -> IO ()
    prepare (_, rawPath) = do
        let path = "_site" ++ rawPath
        chapter <- TIO.readFile path
        let chapter'  = T.replace "SOLUTION_BEGIN" solutionBegin chapter
            chapter'' = T.replace "SOLUTION_END"   solutionEnd   chapter'
        TIO.writeFile path chapter''

solutionBegin, solutionEnd :: T.Text
solutionBegin = T.pack [r|<div style="padding-top: 10px; padding-bottom: 10px;"><ul class="collapsible collapsible-accordion" data-collapsible="accordion"><li><div class="collapsible-header"><i class="fa fa-check-square-o" style="font-size: 21px; color: green;" aria-hidden="true"></i><span class="sans">Решение</span></div><div class="collapsible-body"><div class="solution-content">|]
solutionEnd = T.pack [r|</div></div></li></ul></div>|]

