{-# LANGUAGE OverloadedStrings #-}

module CreateCss (
    createCss
) where

import           Prelude            hiding (span, div)
import           Clay
import qualified Data.Text.Lazy     as L

createCss :: IO ()
createCss = writeFile "static/css/default.css" . L.unpack . render $ do
    let colorBase01   = color "#586e75"
        colorBase03   = color "#002b36"
        base2         = "#eee8d5"
        solarizedBlue = "#268bd2"
        centerAlign   = textAlign . alignSide $ sideCenter
        leftAlign     = textAlign . alignSide $ sideLeft
        justifyAlign  = textAlign justify

        paddingTopPx    = paddingTop . px
        paddingBottomPx = paddingBottom . px
        paddingLeftPx   = paddingLeft . px
        paddingRightPx  = paddingRight . px

        marginTopPx     = marginTop . px
        marginBottomPx  = marginBottom . px

        fontSizePx      = fontSize . px
        fontSizePct     = fontSize . pct

        borderRadiusPx tL tR bR bL = borderRadius (px tL) (px tR) (px bR) (px bL)

        monoFont = fontFamily ["Ubuntu Mono"] [monospace]

        hsCodeBase = do
            color           "#000"
            backgroundColor base2
            monoFont
            fontSizePct     100
            borderRadiusPx  3 3 3 3
            border          solid (px 1) base2

        hsCode = code <? do
            hsCodeBase
            paddingTopPx    1
            paddingBottomPx 1
            paddingLeftPx   3
            paddingRightPx  3

        hrefWithArrow = do
            content . stringContent $ "⇗"
            paddingLeftPx   2
            fontSizePx      14

    importUrl "https://fonts.googleapis.com/css?family=PT+Sans:400|PT+Serif:400,700,400italic|Ubuntu+Mono:400,400italic,700&subset=latin,cyrillic"

    body ? do
        colorBase03
        fontSizePx      17
        fontFamily      ["PT Serif"] [serif]
        backgroundColor "#fdf6e3"
        justifyAlign

    h1 ? do
        colorBase01
        fontSizePx      30
        centerAlign
        paddingTopPx    40
        paddingBottomPx 35

    h2 ? do
        colorBase01
        fontSizePx      22
        leftAlign
        paddingTopPx    30
        paddingBottomPx 20

    h3 ? do
        colorBase01
        fontSizePx      20
        leftAlign
        paddingTopPx    15
        paddingBottomPx 10

    p    ? hsCode
    span ? hsCode
    li   ? hsCode

    a ? color solarizedBlue
    a # hover   ? textDecoration none
    a # visited ? textDecoration none
    a # active  ? textDecoration none

    p  |>       a # after ? hrefWithArrow
    ol |> li |> a # after ? hrefWithArrow

    blockquote ? do
        marginLeft      nil
        paddingLeftPx   20
        color           "#666"
        borderLeft      solid (px 5) "#ee6e73"

    ".cover-title" ? do
        colorBase01
        fontSizePx      48
        paddingTopPx    15
        marginBottomPx  30

    ".book-icon" ? do
        fontSizePx      130
        opacity         0.5
        color           "#448aff"

    ".keyboard-icon" ? do
        fontSizePx      130
        opacity         0.5
        color           "#ffa000"

    ".publish-version" ? do
        colorBase01
        fontSizePx      18
        paddingBottomPx 40

    ".read-button" ? do
        paddingTopPx    40
        paddingBottomPx 20

    ".practice-button" ? do
        paddingTopPx    40
        paddingBottomPx 40

    ".sans" ?
        fontFamily ["PT Sans"] [sansSerif]

    ".mono" ?
        monoFont

    ".get-button" ?
        width (px 148)

    ".get-button-separator" ?
        paddingBottomPx 25

    ".smaller-text" ?
        fontSizePct 100

    ".side-nav" ? do
        a ? do
            leftAlign
            fontSizePx 17
        li ?
            padding  nil nil nil nil

    nav ? do
        opacity         0.9
        backgroundColor "#2aa198"
        ".brand-logo" ?
            fontSizePx 26

        ul ? a ?
            fontSizePx 26

    div # "#logo" ?
        ".navbar-brand" ?
            fontSizePx 16

    ".navbar" ? a ? do
        borderBottom    solid nil "#000"
        textDecoration  none

    ".navbar-header" ? a ? do
        borderBottom    solid nil "#000"
        textDecoration  none

    ".navbar-default" ? do
        backgroundColor "#fff"
        borderColor     "#e7e7e7"
        opacity         0.91

        ".navbar-nav" |> li |> a # hover ?
            color "#222"

        ".navbar-nav" |> li |> a # focus ?
            color "#222"

    ".author" ?
        paddingLeftPx 15

    ".chapter-arrow" ?
        fontSizePx 21

    ".sourceCode" ? code ?
        monoFont

    ".donate" ?
        paddingTopPx 50

    ".donate-button" ? do
        textTransform   none
        color           "#f17603"
        fontSizePx      18

    ".donate-area" ?
        centerAlign

    ".solution-content" ? do
        padding         (px 20) (px 20) (px 20) (px 20)
        backgroundColor "#FEFBF3"

    ".collapsible-body" ?
        p ? padding     (px 0) (px 0) (px 0) (px 0)

    pre # ".sourceCode" ? do
        hsCodeBase
        padding         (px 7) (px 7) (px 7) (px 7)
        marginTopPx     18
        marginBottomPx  18

        span # ".kw" ? do color "#007020"; fontWeight bold
        span # ".dt" ? color "#902000"
        span # ".dv" ? color "#40a070"
        span # ".bn" ? color "#40a070"
        span # ".fl" ? color "#40a070"
        span # ".ch" ? color "#4070a0"
        span # ".st" ? color "#4070a0"
        span # ".co" ? do color "#60a0b0"; fontStyle italic
        span # ".ot" ? color "#007020"
        span # ".al" ? do color red; fontWeight bold
        span # ".fu" ? color "#06287e"
        span # ".er" ? do color red; fontWeight bold

