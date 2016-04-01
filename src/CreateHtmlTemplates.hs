{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module CreateHtmlTemplates (
    createHtmlTemplates
) where

import           Prelude                            hiding (div, span)
import           Text.Blaze.Html5                   as H
import           Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty    (renderHtml)
import qualified Data.Text                          as T
import           Text.RawString.QQ

import           SingleMarkdown
import           SubjectIndex

createHtmlTemplates :: [ChapterPoint] -> IO ()
createHtmlTemplates chapterPoints = do
    writeFile "templates/cover.html"         $ renderHtml   createCover
    writeFile "templates/default.html"       $ renderHtml $ createDefault chapterPoints
    writeFile "templates/chapter.html"       $ renderHtml   createChapter
    writeFile "templates/subject-index.html" $ renderHtml $ createSubjectIndex chapterPoints
    writeFile "templates/donate.html"        $ renderHtml   createDonate

data Donate = Ask | Skip

createCover :: Html
createCover = docTypeHtml ! lang "ru" $ do
    commonHead "О Haskell по-человечески"

    body $ do
        nav $
            div ! class_ "nav-wrapper" $ do
                div ! class_ "left author sans" $ do
                    preEscapedToHtml ("&copy; 2016&nbsp;" :: String)
                    a ! href "http://dshevchenko.biz" ! target "_blank" $
                        "Д. Шевченко"

                contacts Ask

        div ! class_ "container" $
            div ! class_ "row center" ! A.style "padding-top: 70px;" $ do
                div ! class_ "cover-title" $ do
                    toHtml ("Про Haskell по-людськи" :: String)
                    H.span ! A.style "font-size: 28px; color: orangered;" $
                        preEscapedToHtml ("&nbsp;&beta;" :: String)

                div ! class_ "cover-v2" $
                    "видання 2.0"

                div ! class_ "row" $ do
                    div ! class_ "col s12 m1 l1" $
                        preEscapedToHtml ("&nbsp;" :: String)

                    div ! class_ "col s12 m10 l10" $
                        div ! class_ "row" $ do
                            div ! class_ "col s12 l3" $ do
                                a ! class_ "waves-effect waves-light btn btn-large blue accent-2 get-button sans"
                                  ! href "/init.html" $ do
                                    H.span ! class_ "sans" ! A.style "text-transform: none;" $ "Web"
                                    H.span ! A.style "padding-right: 10px;" $ ""
                                    H.i ! class_ "fa fa-cloud" ! A.style "font-size: 20px;" $ ""
                                div ! class_ "get-button-separator" $ ""

                            div ! class_ "col s12 l3" $ do
                                a ! class_ "waves-effect waves-light btn btn-large red darken-1 get-button sans"
                                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell.pdf?raw=true" $ do
                                    H.span ! class_ "sans" $ "PDF"
                                    H.span ! A.style "padding-right: 10px;" $ ""
                                    H.i ! class_ "fa fa-desktop" ! A.style "font-size: 20px;" $ ""
                                div ! class_ "get-button-separator" $ ""

                            div ! class_ "col s12 l3" $ do
                                a ! class_ "waves-effect waves-light btn btn-large red lighten-1 get-button sans"
                                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell-mobile.pdf?raw=true" $ do
                                    H.span ! class_ "sans" $ "PDF"
                                    H.span ! A.style "padding-right: 10px;" $ ""
                                    H.i ! class_ "fa fa-tablet" ! A.style "font-size: 20px;" $ ""
                                div ! class_ "get-button-separator" $ ""

                            div ! class_ "col s12 l3" $ do
                                a ! class_ "waves-effect waves-light btn btn-large light-green darken-1 get-button sans"
                                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/epub/ohaskell.epub?raw=true" $ do
                                    H.span ! class_ "sans" $ "EPUB"
                                    H.span ! A.style "padding-right: 10px;" $ ""
                                    H.i ! class_ "fa fa-book" ! A.style "font-size: 20px;" $ ""
                                div ! class_ "get-button-separator" $ ""

                    div ! class_ "col s12 m1 l1" $
                        preEscapedToHtml ("&nbsp;" :: String)

createDefault :: [ChapterPoint] -> Html
createDefault chapterPoints = docTypeHtml ! lang "ua" $ do
    commonHead "PAGE_TITLE <- Про Haskell по-людськи"

    body $ do
        div ! class_ "navbar-fixed" $
            nav $
                div ! class_ "nav-wrapper" $ do
                    hashtag
                    navigation chapterPoints
                    contacts Skip

        div ! class_ "container" $
            preEscapedToHtml ("$body$" :: String)

createChapter :: Html
createChapter = do
    preEscapedToHtml ("$body$" :: String)

    div ! A.style "padding-top: 45px;" $ ""

    div ! class_ "row" $ do
        div ! class_ "col s3" $
            div ! class_ "left" $
                a ! href "PREV_CHAPTER_URL"
                  ! class_ "btn waves-effect waves-light chapter-arrow" $
                    H.span ! class_ "fa fa-angle-double-left" $ ""

        div ! class_ "col s6" $
            div ! class_ "center-align" $
                button ! class_ "waves-effect waves-light btn blue lighten-2 show-comments sans" $
                    "Обговоримо?"

        div ! class_ "col s3" $
            div ! class_ "right" $
                a ! href "NEXT_CHAPTER_URL"
                  ! class_ "btn waves-effect waves-light chapter-arrow" $
                    H.span ! class_ "fa fa-angle-double-right" $ ""

    div ! A.style "padding-top: 20px;" $ ""

    -- The empty element required for Disqus to loads comments into
    div ! A.id "disqus_thread" ! A.style "padding-top: 30px;" $ ""

createSubjectIndex :: [ChapterPoint] -> Html
createSubjectIndex chapterPoints = docTypeHtml ! lang "ru" $ do
    commonHead "Предметний індекс <- Про Haskell по-людськи"

    body $ do
        div ! class_ "navbar-fixed" $
            nav $
                div ! class_ "nav-wrapper" $ do
                    hashtag
                    navigation chapterPoints
                    contacts Skip

        div ! class_ "container" $ do
            H.h1 "Предметний індекс"

            div ! class_ "subject-index-wrapper" $ do
                mapM_ subjectPoint $ subjectIndexWithHrefs chapterPoints

        div ! A.style "padding-bottom: 30px;" $ ""

subjectPoint :: (SubjectName, [HrefWithLabel]) -> Html
subjectPoint (subjectName, hrefs) =
    div ! class_ "row" $ do
        div ! class_ "col s12 m6 l6" $
            H.span ! class_ "" $ toHtml subjectName
        div ! class_ "col s12 m6 l6" $ do
            mapM_ subjectOneHref hrefs
  where
    subjectOneHref :: HrefWithLabel -> Html
    subjectOneHref (aHref, aLabel) = do
        a ! class_ "mono"
          ! href (toValue aHref) $
            toHtml aLabel

        H.span $ preEscapedToHtml ("&nbsp;&nbsp;" :: String)

createDonate :: Html
createDonate = docTypeHtml ! lang "ru" $ do
    commonHead "Підтримати <- Про Haskell по-людськи"

    body $ do
        div ! class_ "navbar-fixed" $
            nav $
                div ! class_ "nav-wrapper" $ do
                    hashtag
                    contacts Skip

        div ! class_ "container" $ do
            H.h1 "Підтримати проект"

            div ! class_ "donate-area" $ do
                H.h3 ! class_ "center-align" $
                    preEscapedToHtml ("Яндекс.Деньги" :: String)
                preEscapedToHtml yandexMoneyForm

                H.h3 ! class_ "center-align" $
                    preEscapedToHtml ("PayPal" :: String)
                preEscapedToHtml payPalDonateForm

            H.h3 ! class_ "center-align" $ "Дякую вам!"

commonHead :: T.Text -> Html
commonHead customTitle = H.head $ do
    meta ! charset "utf-8"
    meta ! name "description" ! content "Про Haskell по-людськи. Ваша перша книга про прекрасну и дивовижну мову програмування."
    meta ! name "author" ! content "Денис Шевченко"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1.0"

    H.title $ toHtml customTitle

    link ! rel "icon" ! href "/static/images/favicon.ico"
    link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js" $ ""
    link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js" $ ""
    link ! rel "stylesheet" ! href "/static/css/default.css"
    script ! src "/static/js/default.js" $ ""

-- Контакты для верхнего правого угла страницы.
contacts :: Donate -> Html
contacts donate =
    ul ! A.id "nav-mobile" ! class_ "right" $ do
        li $
            a ! href "https://github.com/denisshevchenko/ohaskell.guide"
              ! target "_blank" $
                H.span ! class_ "fa fa-github" $ ""
        li $
            a ! href "mailto:me@dshevchenko.biz?Subject=#ohaskell,%20О%20книге"
              ! A.title "Написати автору" $
                H.span ! class_ "fa fa-envelope-o" $ ""
        addDonate
  where
    addDonate = case donate of
        Ask  -> li $
                    a ! href "/donate.html"
                      ! A.title "Підтримати" $
                        H.span ! class_ "fa fa-rub" ! A.style "font-size: 23px;" $ ""
        Skip -> return ()

hashtag :: Html
hashtag =
    a ! class_ "brand-logo center sans"
      ! href "/" $ do
        preEscapedToHtml ("#ohaskell" :: String)
        H.span ! A.style "font-size: 19px; color: yellow;" $
            preEscapedToHtml ("&beta;" :: String)

-- Оглавление и Предметный указатель для верхнего левого угла.
navigation :: [ChapterPoint] -> Html
navigation chapterPoints = do
    ul ! A.id "nav-mobile" ! class_ "left" $ do
        li $
            a ! href "#"
              ! dataAttribute "activates" "mobile-demo"
              ! class_ "button-collapse show-on-large" $
                H.span ! class_ "fa fa-list-ul" ! A.style "font-size: 26px;" $ ""
        li $
            a ! href "/subject-index.html" $
                H.span ! class_ "fa fa-tags" ! A.style "font-size: 22px;" $ ""

    -- Строим левое боковое оглавление.
    ul ! class_ "side-nav sans" ! A.id "mobile-demo" $
        mapM_ chapterPoint chapterPoints
  where
    chapterPoint :: (ChapterName, ChapterPath) -> Html
    chapterPoint (aName, anUrl) = li $ a ! href (stringValue anUrl) $ toHtml aName

yandexMoneyForm :: String
yandexMoneyForm = [r|
<iframe frameborder="0" allowtransparency="true" scrolling="no" src="https://money.yandex.ru/embed/donate.xml?account=410012187867374&quickpay=donate&payment-type-choice=on&mobile-payment-type-choice=on&targets=%D0%A0%D0%B0%D0%B7%D0%B2%D0%B8%D1%82%D0%B8%D0%B5+%D0%BA%D0%BD%D0%B8%D0%B3%D0%B8&target-visibility=on&project-name=%D0%9E+Haskell+%D0%BF%D0%BE-%D1%87%D0%B5%D0%BB%D0%BE%D0%B2%D0%B5%D1%87%D0%B5%D1%81%D0%BA%D0%B8&project-site=http%3A%2F%2Fwww.ohaskell.guide%2F&button-text=05&successURL=" width="508" height="117"></iframe>|]

payPalDonateForm :: String
payPalDonateForm = [r|
<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
    <input type="hidden" name="cmd" value="_s-xclick">
    <input type="hidden" name="hosted_button_id" value="6JWPGQPDWY2X6">
    <input type="image" src="https://www.paypalobjects.com/ru_RU/RU/i/btn/btn_donateCC_LG.gif" border="0" name="submit" alt="PayPal — более безопасный и легкий способ оплаты через Интернет!">
    <img alt="" border="0" src="https://www.paypalobjects.com/ru_RU/i/scr/pixel.gif" width="1" height="1">
</form>|]

