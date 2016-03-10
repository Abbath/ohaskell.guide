# Первые вопросы

С них и начнём.

## &laquo;Что такое этот ваш Haskell?&raquo;

Haskell &mdash; чисто функциональный язык программирования общего назначения, может быть использован для решения самого широкого круга задач. Компилируемый, но может вести себя и как скриптовый. Кроссплатформенный. Ленивый, со строгой статической типизацией. И он не похож на другие языки. Совсем.

## &laquo;Это что, какой-то новый язык?&raquo;

Вовсе нет. История Haskell началась ещё в 1987 году. Этот язык был рождён в математических кругах, когда группа людей решила создать лучший фукнциональный язык программирования. В 1990 году вышла первая версия языка, названного в честь известного американского математика [Хаскела Карри](https://en.wikipedia.org/wiki/Haskell_Curry). В 1998 году язык был стандартизован, а начиная с 2000-х началось его медленное вхождение в мир практического программирования. За эти годы язык совершенствовался, и вот в 2010 мир увидел его обновлённый стандарт. Так что мы имеем дело вовсе не с молоденьким выскочкой.

## &laquo;И кто его сделал?&raquo;

Главная реализация языка нашла своё воплощение в компиляторе GHC (The Glasgow Haskell Compiler), родившемся в недрах Microsoft Research. Впрочем, отнеситесь к слову &laquo;Microsoft&raquo; спокойно &mdash; к `.NET` и прочим известным творениям Компании-из-Редмонда компилятор GHC отношения не имеет.

## &laquo;А библиотеки для Haskell имеются?&raquo;

Имеются, и весьма много. В процессе чтения вы познакомитесь со многими из них.

## &laquo;Да, но я слышал, что Haskell ещё не готов к production...&raquo;

Готов, и уже не первый год. С момента выхода первого стандарта язык улучшался, развивалась его экосистема, появлялись новые библиотеки, выходили в свет книги. Сегодня, в 2016, можно уверенно заявить, что Haskell полностью готов к серьёзному коммерческому использованию, о чём убедительно свидетельствуют истории успешного внедрения Haskell в бизнесе, в том числе крупном.

## &laquo;А правда ли, что порог вхождения в Haskell высок?"

Правда. Haskell настолько не похож на другие языки, что людям, пришедшим из мира других языков, мозги поломать придётся. Именно поломать, а не просто пошевелить ими: Haskell заставляет иначе взглянуть на, казалось бы, привычные вещи. В этом его сложность и в этом же его красота: многие люди, включая меня, узнав вкус Haskell, категорически не желают возвращаться к другим языкам. Я вас предупредил.

## &laquo;И что же в нём такого необычного?&raquo;

Например, в Haskell нет оператора присваивания. Вообще. А что касается остальных странностей языка &mdash; вся книга им и посвящена.

## &laquo;А если сравнить его с C++/Python/Scala...&raquo;

Сравнение Haskell с другими языками выходит за рамки этой книги. Несколько раз вы встретите здесь кусочки кода на других языках, но я привожу их исключительно для того, чтобы подчеркнуть различие с Haskell, а вовсе не для сравнения в контексте &laquo;лучше/хуже&raquo;.
