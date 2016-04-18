{-# LANGUAGE OverloadedStrings #-}

module CreateEpubCss (
    createEpubCss
) where

import           Prelude            hiding (span, div)
import           Clay
import qualified Data.Text.Lazy     as L

createEpubCss :: FilePath -> IO ()
createEpubCss pathToCss = writeFile pathToCss . L.unpack . render $ do
    let centerAlign     = textAlign $ alignSide sideCenter
        leftAlign       = textAlign $ alignSide sideLeft

        paddingTopPx    = paddingTop . px
        paddingBottomPx = paddingBottom . px
        paddingLeftPx   = paddingLeft . px

        fontSizePx      = fontSize . px

        fontPTSerif     = fontFamily ["PT Serif"] [serif]
        fontUbuntuMono  = fontFamily ["Ubuntu Mono"] [monospace]

        fontFaceByName aName = fontFaceSrc [FontFaceSrcUrl aName Nothing]

        hrefWithArrow = do
            content . stringContent $ "⇗"
            paddingLeftPx 2
            fontSizePx    9

    fontFace $ do
        fontPTSerif
        fontStyle       normal
        fontWeight      normal
        fontFaceByName  "PTSerif.ttc"

    fontFace $ do
        fontPTSerif
        fontStyle       normal
        fontWeight      bold
        fontFaceByName  "PTSerif.ttc"

    fontFace $ do
        fontUbuntuMono
        fontStyle       normal
        fontWeight      normal
        fontFaceByName  "UbuntuMono-Regular.ttf"

    fontFace $ do
        fontUbuntuMono
        fontStyle       normal
        fontWeight      bold
        fontFaceByName  "UbuntuMono-Bold.ttf"

    body ? do
        margin          (pct 5) (pct 5) (pct 5) (pct 5)
        textAlign       justify
        fontSizePx      11
        fontPTSerif

    code ?
        fontUbuntuMono

    h1 ? do
        leftAlign
        fontSizePx      20
        paddingBottomPx 18

    h2 ? do
        leftAlign
        fontSizePx      15
        paddingTopPx    18

    h3 ?
        leftAlign

    h1 # ".main" ? do
        centerAlign
        fontSizePx      26
        paddingTopPx    50

    h1 # ".edition" ? do
        centerAlign
        fontSizePx      13
        paddingBottomPx 50

    h2 # ".author" ? do
        centerAlign
        fontSizePx 13

    ol # ".toc" ? do
        padding         nil nil nil nil
        marginLeft      (em 1)

    ol # ".toc" ? li ? do
        listStyleType none
        margin      nil nil nil nil
        padding     nil nil nil nil

    p |> a # after ? do
        content . stringContent $ "⇗"
        paddingLeftPx   2
        fontSizePx      9

    p  |>       a # after ? hrefWithArrow
    ol |> li |> a # after ? hrefWithArrow

