{-# LANGUAGE OverloadedStrings #-}

module SubjectIndex (
      subjectIndexWithHrefs
    , SubjectName
    , Href
    , HrefWithLabel
) where

import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Numeral.Roman     (toRoman)

import           SingleMarkdown

type SectionName = T.Text
type SubjectName = T.Text
type Link        = (ChapterName, SectionName)
type SubjectItem = (SubjectName, [Link])

type Href = T.Text
type HrefWithLabel = (Href, T.Text)
type SubjectItemWithHref = (SubjectName, [HrefWithLabel])

------------------------------------------------------------------------

subjectIndex :: [SubjectItem]
subjectIndex =
  [
      ("case",                              [ ("Вибір і зразки", "case")
                                            ]
      )

    , ("Hackage",                           [ ("Hackage і бібліотеки", "Hackage")
                                            ]
      )

    , ("if",                                [ ("Вибираємо і повертаємося", "Вибір і вихід")
                                            , ("Вибір і зразки", "Не тільки з двох")
                                            ]
      )

    , ("let",                               [ ("Нехай буде там, Де...", "Нехай")
                                            ]
      )

    , ("stack",                             [ ("Приготуймося", "Встановлюємо")
                                            ]
      )

    , ("where",                             [ ("Нехай буде там, Де...", "Де")
                                            ]
      )

    , ("арифметична послідовність",         [ ("Список", "Переліки")
                                            ]
      )

    , ("АТД",                               [ ("АТД", "")
                                            ]
      )

    , ("виведення типу",                    [ ("Кити і Черепаха", "Вивід")
                                            ]
      )

    , ("вираз",                             [ ("Кити і Черепаха", "Черепаха")
                                            ]
      )

    , ("історія Haskell",                   [ ("Перші питання", "Це що, якась нова мова")
                                            ]
      )

    , ("клас типів",                        [ ("Кити і Черепаха", "Третій Кит")
                                            ]
      )

    , ("конструктор значення",              [ ("Наші типи", "Знайомство")
                                            ]
      )

    , ("кортеж",                            [ ("Кортеж", "")
                                            ]
      )

    , ("лямбда-функція",                    [ ("Лямбда-функція", "Витоки")
                                            , ("ФВП", "Часткове застосування")
                                            , ("Композиція функцій", "Як працює композиція")
                                            ]
      )

    , ("модуль",                            [ ("Приготуймося", "Модулі: знайомство")
                                            , ("Hackage і бібліотеки", "")
                                            ]
      )

    , ("нульарний конструктор",             [ ("Наші типи", "Значення-пустушка")
                                            ]
      )

    , ("оголошення функції",                [ ("Незмінність і чистота", "Оголошуємо і визначаємо")
                                            ]
      )

    , ("визначення  функції",               [ ("Незмінність і чистота", "Оголошуємо і визначаємо")
                                            ]
      )

    , ("оператор",                          [ ("Світ Операторів", "")
                                            ]
      )

    , ("оператор присвоювання",             [ ("Незмінність і чистота", "Чисто функціональний")
                                            ]
      )

    , ("патерн матчінг",                    [ ("Вибір і зразки", "Порівняння зі зразком")
                                            , ("Кортеж", "Дії над кортежами")
                                            , ("АТД", "Витягаємо значення")
                                            , ("АТД: поля з мітками", "Без міток")
                                            ]
      )

    , ("сильна типізація",                  [ ("Кити і Черепаха", "Сила")
                                            ]
      )

    , ("порівняння зі зразком",             [ ("Вибір і зразки", "Порівняння зі зразком")
                                            , ("Кортеж", "Дії над кортежами")
                                            , ("АТД", "Витягаємо значення")
                                            , ("АТД: поля з мітками", "Без міток")
                                            ]
      )

    , ("список",                            [ ("Список", "")
                                            ]
      )

    , ("списковий діапазон",                [ ("Список", "Переліки")
                                            ]
      )

    , ("статична типізація",                [ ("Кити і Черепаха", "Статична перевірка")
                                            ]
      )

    , ("тип",                               [ ("Кити і Черепаха", "Другий Кит")
                                            , ("Наші типи", "Знайомство")
                                            ]
      )

    , ("функція вищого порядку",            [ ("ФВП", "")
                                            ]
      )

    , ("часткове застосування",             [ ("ФВП", "Часткове застосування")
                                            ]
      )
  ]

------------------------------------------------------------------------

subjectIndexWithHrefs :: [ChapterPoint] -> [SubjectItemWithHref]
subjectIndexWithHrefs chapterPoints = map (create chapterPoints) subjectIndex

create :: [ChapterPoint] -> SubjectItem -> SubjectItemWithHref
create chapterPoints (subjectName, links) = (subjectName, V.toList hrefsWithLabels)
  where
    hrefs           = map (createHrefFromLink chapterPoints) links
    hrefsWithIndex  = V.indexed . V.fromList $ hrefs
    hrefsWithLabels = V.map makeLabel hrefsWithIndex

makeLabel :: (Int, Href) -> HrefWithLabel
makeLabel (index, href) = (href, roman)
  where
    roman = toRoman (index + 1) :: T.Text

createHrefFromLink :: [ChapterPoint] -> Link -> Href
createHrefFromLink chapterPoints (chapterName, sectionName) =
    case lookup chapterName chapterPoints of
        Nothing  -> error $ "No such chapter '" ++ T.unpack chapterName ++ "', abort!"
        Just url -> T.pack url `T.append` "#" `T.append` createIdFrom sectionName
  where
    createIdFrom aSectionName = T.strip . T.toLower $ T.replace " " "-" aSectionName

