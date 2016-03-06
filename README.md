[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ruHaskell/forall)

О Haskell по-человечески
========================

Ваша первая книга об удивительном и прекрасном языке программирования Haskell.

Доступна как [онлайн](http://www.ohaskell.guide/), так и в виде [PDF](https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell.pdf?raw=true) и [EPUB](https://github.com/denisshevchenko/ohaskell.guide/blob/master/epub/ohaskell.epub?raw=true).

Книга создана с помощью блистательных [Materialize](http://materializecss.com/), [Hakyll](https://jaspervdj.be/hakyll/), [Clay](http://fvisser.nl/clay/), [BlazeHtml](https://jaspervdj.be/blaze/) и [pandoc](http://pandoc.org/).

### ruHaskell

Книга написана при поддержке русскоязычного сообщества Haskell-разработчиков. Мы живём здесь:

1. [Основной сайт](http://ruhaskell.org/)
2. [Чат](https://gitter.im/ruHaskell/forall)
3. [/r/ruhaskell](https://www.reddit.com/r/ruhaskell/)
4. [Twitter](https://twitter.com/ruHaskell)
5. [Google+](https://plus.google.com/communities/117343381540538069054)

Вопросы, предложения и критику просьба направлять [прямиком мне](mailto:me@dshevchenko.biz?Subject=#ohaskell,%20О%20книге).

### Локальная сборка

Вы можете собрать книгу локально. Подразумевается, что у вас уже есть `stack` и каталог `~/.local/bin` уже добавлен в ваш `PATH`. Выполняем:

```bash
$ git clone git@github.com:denisshevchenko/ohaskell.guide.git
$ cd ohaskell.guide
$ stack install
$ ohaskell rebuild
```

В результате сборки три варианта книги вы найдёте здесь:

1. HTML = _site/index.html
2. PDF  = pdf/ohaskell.pdf
3. EPUB = epub/ohaskell.epub

