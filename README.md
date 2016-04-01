[![CircleCI](https://circleci.com/gh/denisshevchenko/ohaskell.guide.svg?style=shield&circle-token=42b4b253957b4896ad05759fce3a7ae576ac8a72)](https://circleci.com/gh/denisshevchenko/ohaskell.guide)&nbsp;&nbsp;&nbsp;[![Code Climate](https://codeclimate.com/github/denisshevchenko/ohaskell.guide/badges/gpa.svg)](https://codeclimate.com/github/denisshevchenko/ohaskell.guide)&nbsp;&nbsp;&nbsp;[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/denisshevchenko/ohaskell-book)

# Про Haskell по-людськи

Ваша перша книга про дивовижну і прекрасну мову програмування [Haskell](https://www.haskell.org/).

[![readOnline](https://img.shields.io/badge/read-online-blue.svg)](http://www.ohaskell.guide/init.html)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getPDF](https://img.shields.io/badge/get-PDF-red.svg)](https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell.pdf?raw=true)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getPDFMobile](https://img.shields.io/badge/get-PDF%20mobile-orange.svg)](https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell-mobile.pdf?raw=true)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getEPUB](https://img.shields.io/badge/get-EPUB-green.svg)](https://github.com/denisshevchenko/ohaskell.guide/blob/master/epub/ohaskell.epub?raw=true)

Книга створена за допомогою практичного [Markdown](https://help.github.com/categories/writing-on-github/), блискучого [Materialize](http://materializecss.com/), вражаючого [Hakyll](https://jaspervdj.be/hakyll/), елегантного [Clay](http://fvisser.nl/clay/), гнучкого [BlazeHtml](https://jaspervdj.be/blaze/) и потужного [pandoc](http://pandoc.org/). Звісно, все це звязано во єдине ціле силою Haskell. Книга написана при поідтримці [російськомовної спільноти Haskell-розробників](http://ruhaskell.org/).

## Розповсюдження

Книга вільно розповсюджується на умовах ліцензії [CC BY-NC 4.0](http://creativecommons.org/licenses/by-nc/4.0/deed.ru). Вихідний програмний код ще більш вільно розповсюджується на умовах ліцензії [MIT](https://opensource.org/licenses/MIT).

## Новини

За новинами про оновлення та виправлення книги стежте в [нашому чаті](https://gitter.im/denisshevchenko/ohaskell-book), а також у випусках подкасту [Банани та Лінзи](http://bananasandlenses.net/), єдиного російськомовного подкасту, цілком присвяченого Haskell. Ну й [Твіттер мій](https://twitter.com/dshevchenko_biz) можете переглядати.

## Локальна збірка

Для локальної збірки вам знадобляться [stack](http://docs.haskellstack.org/en/stable/README/), [pandoc](http://pandoc.org/) і TeX-дистрибутив (я ивикористовую [MacTeX](https://tug.org/mactex/)). Мається на увазі, що каталог `~/.local/bin` вже доданий в ваш `PATH`. Робимо:

```bash
$ git clone git@github.com:Abbath/ohaskell.guide.git
$ cd ohaskell.guide
$ stack install
$ ohaskell rebuild
```

Результат збірки:

1. HTML: `_site/index.html`
2. PDF: `pdf/ohaskell.pdf`
3. PDF, мобільний варіант: `pdf/ohaskell-mobile.pdf`
4. EPUB: `epub/ohaskell.epub`

Перевірено на OS X Yosemite, [stack 1.0.2](http://docs.haskellstack.org/en/stable/README/), [pandoc 1.15.2.1](https://github.com/jgm/pandoc/releases/tag/1.15.2) та [LTS Haskell 5.5](https://www.stackage.org/lts-5.5).
