# Про цю книгу

В останні роки помітно зросла кількість книг, присвячених Haskell, і це тішить. Кожна з них переслідує свою мету, тому важко сказати, яка з них краща. Мета цієї книги двояка.

По-перше, я навчу вас головному в Haskell. Основ, без освоєння яких рухатися далі ніяк не вийде.

По-друге, я зруйную страх. Вже багато років довкола Haskell витає дух страху, і я сповна відчув його на собі. Насправді Haskell зовсім не страшний, в ньому немає чорної магії, і щоб програмувати на ньому, вам не потрібен вчений ступінь. Більш того, ви здивуєтеся, наскільки просто в Haskell робити багато речей, але ця простота відкриється вам лише після того, як ви близько познайомитеся з Трьома Китами Haskell, а також з пані Черепахою, що підтримує їх. Імена цих Китів і Черепахи ви дізнаєтеся вже в наступній главі.

Ця книга не заведе вас на вершини Haskell, але вона відкриє вам шлях до цих вершин.

## Чого тут немає

Трьох речей, які ви не знайдете на сторінках цієї книги:

1. Вичерпного довідника по Haskell. Дублювати [офіційний опис стандарту Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/) я не стану.
2. Набору готових рецептів. За рецептами завітайте на [Stackoverflow](http://stackoverflow.com/questions/tagged/haskell).
3. Введення в математичну теорію. Незважаючи на те, що Haskell корінням своїм йде в математику, у цій книзі немає занурення в теорію категорій та інші теорії. Вибачте, якщо розчарував.

## Про перше та друге видання

На обкладинці ви бачили мітку &laquo;видання 2.0&raquo;. Перед вами друге видання, повністю перероблене та переосмислене. Ось дві причини, що спонукали мене переписати книгу.

Перша &mdash; мої помилки. Я переконаний, що навчати мови програмування можуть лише ті, хто використовує цю мову у своїй щоденній роботі. На момент написання першої версії я ще не працював з Haskell, а тому багато чого не знав і не розумів. В результаті частина інформації з першого видання була відверто бідною, а кілька глав взагалі вводили читача в оману.

Друга причина &mdash; змінилася мета книги. Я навмисно звузив коло розглянутих тут тем. Тепер книга цілком присвячена основам мови, тому не шукайте тут розгляду специфічних тем. Я не дуже-то вірю в ідею book-all-in-one, книга для новачків повинна бути книгою для новачків. Ви не зустрінете тут ні прикладів реалізації 3D-рушія, ні розповіді про роботу з PostgreSQL, ні розповіді про проектування гри для Android. Все це можна робити з Haskell, але подібним темам присвячені інші публікації, які безсумнівно будуть вам до снаги після прочитання моєї книги.

## Читайте послідовно

І це важливо. У процесі читання ви помітите, що я періодично піднімаю питання і неначе залишаю їх без відповіді. Це робиться цілком свідомо: відповіді обов'язково будуть дані, але в наступних розділах, там, де це буде найбільш доречно. Тому перестрибування з розділу на розділ може вас заплутати.

Втім, в веб-версії книги є &laquo;Предметний покажчик&raquo;, який допоможе вам швидко знайти потрібне місце, що особливо корисно при повторному прочитанні книги.

## Для цікавих

Наприкінці більшості розділів ви знайдете невелику секцію, яка так і називається &mdash; &laquo;Для допитливих&raquo;. Читати її не обов'язково, але допитливим неодмінно сподобається. У цій секції я наводжу деякі технічні подробиці, історичні відомості та просто цікаві факти.

І врахуйте, будь ласка: вміст секції &laquo;Для допитливих&raquo; іноді трохи ламає послідовність викладу матеріалу, це зроблено свідомо. Пам'ятаючи про численні питання читачів до розділів з попереднього видання, я виніс відповіді на деякі з цих питань в дану секцію, і тому вона, скажімо, в 12 розділі може посилатися на матеріал, викладений лише в 16 розділі. Якщо сумніваєтеся &mdash; не читайте.

## Про пояснення

У багатьох прикладах вихідного коду ви побачите пояснення ось такого вигляду:

```haskell
type String =     [Char]

тип  цей дорівнює цьому
```

Такі пояснення слід читати зліва направо і зверху вниз, і ви відразу зрозумієте що до чого. Кожна частина пояснення розташована строго під тим шматочком коду, до якого вона належить.

Ось ще один приклад:

```haskell
let (host, alias) = ("173.194.71.106", "www.google.com")

                      дане значення
     це
     хост
                                        а ось це
                                        значення
           це
           ім'я
```

Тут я кажу вам: &laquo;Дане значення &mdash; це хост, а ось це значення &mdash; це ім'я&raquo;. У ряді випадків я використовую також різного виду підкреслення:

```haskell
(host, alias) = ("173.194.71.106", "www.google.com")

____             ________________

       =====                        ================
```

Тут я проводжу паралель: &laquo;Значення `host` асоційоване з рядком `173.194.71.106`, а значення `alias` &mdash; з рядком `www.google.com`&raquo;.

## Подяка

Ця книга &mdash; плід не тільки моїх зусиль. Багато членів нашої спільноти допомогли мені порадами, зауваженнями та виправленнями. Велике спасибі вам, друзі!

А ще я дякую всім тим, хто створив Haskell, і всім тим, хто невпинно вдосконалює його. З вашими зусиллями наша професія стає ще більш прекрасною!

## Слово до тих, хто читав перше видання

Якщо ви не читали його &mdash; можете переходити до наступного розділу.

Як вже було сказано, мета книги змінилася. Я переконаний, що новачкові слід дати фундамент, освоївши який, він зможе самостійно вивчати те, що потрібно саме йому. Я більше не хочу давати читачам рибу, я хочу дати їм вудку. Тому тут немає розповідей про всі наявні монадні трансформери, або про всі контейнери, або про Кметтівські лінзи, або про труби Гонзалеса.

Я зроблю акцент на теорію, але вже глибше. Так, в минулому виданні я часто використовував неточну термінологію, відверто затупив з визначенням монади, прогнав якусь пургу з ФВП, ні словом не обмовився про функторні та інші закони, майже не розповів про патерн матчінг і використовував мало прикладів реального коду. У цьому виданні я постараюся виправити ці помилки.

І я як і раніше відкритий до вашої критики.
