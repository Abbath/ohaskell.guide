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

    , ("let",                               [ ("Хай буде там, Де...", "Нехай")
                                            ]
      )

    , ("stack",                             [ ("Приготуймося", "Встановлюємо")
                                            ]
      )

    , ("where",                             [ ("Хай буде там, Де...", "Де")
                                            ]
      )

    , ("арифметическая последовательность", [ ("Список", "Переліки")
                                            ]
      )

    , ("АТД",                               [ ("АТД", "")
                                            ]
      )

    , ("выведение типа",                    [ ("Кити і Черепаха", "Вивід")
                                            ]
      )

    , ("выражение",                         [ ("Кити і Черепаха", "Черепаха")
                                            ]
      )

    , ("история Haskell",                   [ ("Перші питання", "Це що, якась нова мова")
                                            ]
      )

    , ("класс типов",                       [ ("Кити і Черепаха", "Третій Кит")
                                            ]
      )

    , ("конструктор значения",              [ ("Наші типи", "Знайомство")
                                            ]
      )

    , ("кортеж",                            [ ("Кортеж", "")
                                            ]
      )

    , ("лямбда-функция",                    [ ("Лямбда-функція", "Витоки")
                                            , ("ФВП", "Часткове застосування")
                                            , ("Композиція функцій", "Як працює композиція")
                                            ]
      )

    , ("модуль",                            [ ("Приготуймося", "Модулі: знайомство")
                                            , ("Hackage і бібліотеки", "")
                                            ]
      )

    , ("нульарный конструктор",             [ ("Наші типи", "Значення-пустушка")
                                            ]
      )

    , ("объявление функции",                [ ("Незмінність і чистота", "Оголошуємо і визначаємо")
                                            ]
      )

    , ("определение функции",               [ ("Незмінність і чистота", "Оголошуємо і визначаємо")
                                            ]
      )

    , ("оператор",                          [ ("Світ операторів", "")
                                            ]
      )

    , ("оператор присваивания",             [ ("Незмінність і чистота", "Чисто функціональний")
                                            ]
      )

    , ("патерн матчінг",                    [ ("Вибір і зразки", "Порівняння із зразком")
                                            , ("Кортеж", "Дії над кортежами")
                                            , ("АТД", "Витягаємо значення")
                                            , ("АТД: поля з мітками", "Без міток")
                                            ]
      )

    , ("сильная типизация",                 [ ("Кити і Черепаха", "Сила")
                                            ]
      )

    , ("сравнение с образцом",              [ ("Вибір і зразки", "Порівняння із зразком")
                                            , ("Кортеж", "Дії над кортежами")
                                            , ("АТД", "Витягаємо значення")
                                            , ("АТД: поля з мітками", "Без міток")
                                            ]
      )

    , ("список",                            [ ("Список", "")
                                            ]
      )

    , ("списочный диапазон",                [ ("Список", "Переліки")
                                            ]
      )

    , ("статическая типизация",             [ ("Кити і Черепаха", "Статична перевірка")
                                            ]
      )

    , ("тип",                               [ ("Кити і Черепаха", "Другий Кит")
                                            , ("Наші типи", "Знайомство")
                                            ]
      )

    , ("функция высшего порядка",           [ ("ФВП", "")
                                            ]
      )

    , ("частичное применение",              [ ("ФВП", "Часткове застосування")
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

