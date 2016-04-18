# Немного о типах

Как бы вы улучшили эти определения?

```haskell
pdf :: String
pdf = "--pdf"

pdfPrintable :: String
pdfPrintable = "--pdf-printable"

epub :: String
epub = "--epub"

html :: String
html = "--html"
```

SOLUTION_BEGIN
Здесь напрашивается сокращение. Если подряд идут определения одного типа, мы можем написать так:

```haskell
pdf, pdfPrintable, epub, html :: String
pdf          = "--pdf"
pdfPrintable = "--pdf-printable"
epub         = "--epub"
html         = "--html"
```

Сначала идёт эдакое единое объявление для всех строк, ведь они имеют тип `String`. А затем идут определения. Обратите внимание, что строковые литералы выровнены по левому краю. Опираясь на собственный опыт, я настаиваю на удобстве такого выравнивания. Попробуем иначе:

```haskell
pdf = "--pdf"
pdfPrintable = "-pdf-printable"
epub = "--epub"
html = "--html"
```

Вы заметили? Флаг `"-pdf-printable"` ошибочно написан с одним минусом в начале. При выравнивании заметить такое куда легче:

```haskell
pdf          = "--pdf"
pdfPrintable = "-pdf-printable"
epub         = "--epub"
html         = "--html"
```

Помните: код пишется для людей, а не для компьютеров.
SOLUTION_END

