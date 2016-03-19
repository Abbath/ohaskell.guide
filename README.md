[![Gitter chat](https://img.shields.io/badge/chat-on%20gitter-green.svg)](https://gitter.im/ruHaskell/forall)

О Haskell по-человечески
========================

Ваша первая книга об удивительном и прекрасном языке программирования Haskell. Официальный [сайт](http://www.ohaskell.guide/).

Книга создана с помощью практичного [Markdown](https://help.github.com/categories/writing-on-github/), блистательного [Materialize](http://materializecss.com/), впечатляющего [Hakyll](https://jaspervdj.be/hakyll/), элегантного [Clay](http://fvisser.nl/clay/), гибкого [BlazeHtml](https://jaspervdj.be/blaze/) и мощного [pandoc](http://pandoc.org/). И разумеется, всё это связано воедино силою Haskell.

Вопросы, предложения и критику можете направлять [прямиком мне](mailto:me@dshevchenko.biz?Subject=#ohaskell,%20О%20книге). И я буду чрезвычайно благодарен за ваши [Pull requests](https://github.com/denisshevchenko/ohaskell.guide/pulls).

### Распространение

Книга бесплатна и распространяется на условиях лицензии [CC BY-NC 4.0](http://creativecommons.org/licenses/by-nc/4.0/deed.ru).

### ruHaskell

Книга написана при поддержке русскоязычного сообщества Haskell-разработчиков. Мы живём здесь:

1. [Основной сайт](http://ruhaskell.org/)
2. [Чат](https://gitter.im/ruHaskell/forall)
3. [/r/ruhaskell](https://www.reddit.com/r/ruhaskell/)
4. [Twitter](https://twitter.com/ruHaskell)
5. [Google+](https://plus.google.com/communities/117343381540538069054)

### Локальная сборка

Вы можете собрать книгу локально. Подразумевается, что у вас уже есть `stack` и каталог `~/.local/bin` уже добавлен в `PATH`. Делаем:

```bash
$ git clone git@github.com:denisshevchenko/ohaskell.guide.git
$ cd ohaskell.guide
$ stack install
$ ohaskell rebuild
```

Результаты сборки:

1. HTML -> `_site/index.html`
2. PDF -> `pdf/ohaskell.pdf`
3. PDF, мобильный вариант -> `pdf/ohaskell-mobile.pdf`
3. EPUB -> `epub/ohaskell.epub`

Проверено на OS X Yosemite, [stack 1.0.2](http://docs.haskellstack.org/en/stable/README/) и [LTS Haskell 5.5](https://www.stackage.org/lts-5.5).
