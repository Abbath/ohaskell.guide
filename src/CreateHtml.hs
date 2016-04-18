{-# LANGUAGE OverloadedStrings #-}

module CreateHtml (
    createHtml
) where

import           Hakyll
-- import           Hakyll.Contrib.Hyphenation (hyphenateHtml, russian)
import           Control.Exception          (finally)

import           PrepareHtmlTOC
import           CreateCss
import           SingleMarkdown

createHtml :: [ChapterPoint] -> [ChapterPoint] -> IO ()
createHtml chapterPoints practicePoints = do
    createCss
    hakyll
      (do
        justCopy          "static/images/*"
        justCopy          "static/css/*"
        justCopy          "static/js/*"
        justCopy          "README.md"
        justCopy          "CNAME"
        justCopy          "LICENSE"
        justCopy          "circle.yml"
        justCreateAndCopy ".nojekyll"

        prepareTemplates
        createCoverPage
        createDonatePage
        createSubjectIndexPage
        createChapters
        createPractice
      ) `finally` polishHtml chapterPoints practicePoints

justCopy :: Pattern -> Rules ()
justCopy something = match something $ do
    route   idRoute
    compile copyFileCompiler

justCreateAndCopy :: Identifier -> Rules ()
justCreateAndCopy something = create [something] $ do
    route   idRoute
    compile copyFileCompiler

prepareTemplates :: Rules ()
prepareTemplates = match "templates/*" $ compile templateCompiler

createCoverPage :: Rules ()
createCoverPage = create ["index.html"] $ do
    route idRoute
    compile $ makeItem ""
        >>= loadAndApplyTemplate "templates/cover.html" defaultContext
        >>= relativizeUrls

createSubjectIndexPage :: Rules ()
createSubjectIndexPage = create ["subject-index.html"] $ do
    route idRoute
    compile $ makeItem ""
        >>= loadAndApplyTemplate "templates/subject-index.html" defaultContext
        >>= relativizeUrls

createDonatePage :: Rules ()
createDonatePage = create ["donate.html"] $ do
    route idRoute
    compile $ makeItem ""
        >>= loadAndApplyTemplate "templates/donate.html" defaultContext
        >>= relativizeUrls

createChapters :: Rules ()
createChapters = match chapters $ do
    route $ removeChaptersDirectoryFromURLs
            `composeRoutes` removeChapterNumberFromURLs 3
            `composeRoutes` setExtension "html"
    compile $ pandocCompiler -- >>= hyphenateHtml russian -- Переносы только в русских словах.
                             >>= loadAndApplyTemplate chapterTemplateName defaultContext
                             >>= loadAndApplyTemplate defaulTemplateName defaultContext
                             >>= relativizeUrls
  where
    chapters            = fromGlob "chapters/*.md"
    chapterTemplateName = fromFilePath "templates/chapter.html"
    defaulTemplateName  = fromFilePath "templates/default.html"

createPractice :: Rules ()
createPractice = match practice $ do
    route $ removePracticeDirectoryFromURLs
            `composeRoutes` removeChapterNumberFromURLs 4
            `composeRoutes` addPracticeDirectoryToURLs
            `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate chapterTemplateName defaultContext
                             >>= loadAndApplyTemplate defaulTemplateName defaultContext
                             >>= relativizeUrls
  where
    practice            = fromGlob "practice/*.md"
    chapterTemplateName = fromFilePath "templates/chapter.html"
    defaulTemplateName  = fromFilePath "templates/default-practice.html"

removeChaptersDirectoryFromURLs :: Routes
removeChaptersDirectoryFromURLs = gsubRoute "chapters/" (const "")

removePracticeDirectoryFromURLs :: Routes
removePracticeDirectoryFromURLs = gsubRoute "practice/" (const "")

addPracticeDirectoryToURLs :: Routes
addPracticeDirectoryToURLs = customRoute $ ("practice/" ++) . toFilePath

removeChapterNumberFromURLs :: Int -> Routes
removeChapterNumberFromURLs howMany = customRoute $ drop howMany . toFilePath

