# Нехай буде там, Де...

У цьому розділі ми дізнаємося, як зробити наші функції більш зручними й читабельними.

## Нехай

Розглянемо наступну функцію:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  if | timeInS < 40 -> timeInS + 120
     | timeInS >= 40 -> timeInS + 8 + 120
```

Ми рахуємо час деякої події, і якщо початкове значення менше `40` секунд &mdash; результуючий час збільшується на `120` секунд, в іншому випадку &mdash; ще на `8` секунд більше. Перед нами класичний приклад &laquo;магічних чисел&raquo; (англ. magic numbers), коли зміст конкретних значень приховано за сімома печатями. Що за `40` та `8`? Щоб уникнути цієї проблеми можна ввести тимчасові вирази, і тоді код стане зовсім іншим:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40
      correction = 120
      delta = 8
  in if | timeInS < threshold -> timeInS + correction
        | timeInS >= threshold -> timeInS + delta + correction
```

Ось, зовсім інша справа! Ми позбулися від &laquo;магічних чисел&raquo;, ввівши пояснючі вирази `threshold`, `correction` і `delta`. Конструкція `let-in` вводить пояснювальні вирази за схемою:

```haskell
let DECLARATIONS in EXPRESSION
```

де `DECLARATIONS` &mdash; вирази, декларовані нами, а `EXPRESSION` &mdash; вираз, в якому використовується вирази з `DECLARATION`. Коли ми написали:

```haskell
let threshold = 40
```

ми оголосили: &laquo;Відтепер вираз `threshold` дорівнює виразу `40`&raquo;. Виглядає як присвоювання, але ми вже знаємо, що в Haskell його немає. Тепер вираз `threshold` може замінити собою число `40` всередині виразу, що йде за словом `in`:

```haskell
let threshold = 40
    ...
in if | timeInS < threshold -> ...
      | timeInS >= threshold -> ...
```

Ця конструкція легко читається:

```haskell
let   threshold =           40 ... in ...

нехай цей       буде        цьому  в тому
      вираз     дорівнювати виразу виразі
```

З допомогою ключового слова `let` можна ввести скільки завгодно пояснювальних/проміжних виразів, що робить наш код, по-перше, зрозумілішим, а по-друге, коротшим. Так, у цьому конкретному випадку код став трохи довшим, але в наступних розділах ми побачимо ситуацію, коли проміжні значення скорочують код в рази.

Важливо пам'ятати, що введені конструкцією `let-in` вирази існують лише в рамках виразу, що слідує за словом `in`. Змінимо функцію:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40
    correction = 120
  in if | timeInS < threshold -> timeInS + correction
        | timeInS >= threshold ->
          let delta = 8 in timeInS
                           + delta
                           + correction

              це           існує лише в
              вираз        рамках цього виразу
```

Ми скоротили область видимості проміжного виразу `delta`, зробивши його видимим лише у виразі `timeInS + delta + correction`.

При бажанні `let`-вирази можна записувати і в рядок:

```haskell
...
let threshold = 40; correction = 120
in if | timeInS < threshold -> timeInS + correction
      | timeInS >= threshold ->
        let delta = 8 in timeInS + delta + correction
```

У цьому випадку ми перераховуємо їх через крапку з комою. Особисто мені такий стиль не подобається, але вибирати вам.

## Де

Існує інший спосіб введення проміжних виразів:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  if | timeInS < threshold -> timeInS + correction
     | timeInS >= threshold -> timeInS + delta + correction
  where
    threshold = 40
    correction = 120
    delta = 8
```

Ключове слово `where` робить приблизно те ж, що й `let`, але проміжні вирази задаються в кінці функції. Така конструкція читається подібно до наукової формули:

```haskell
  S = V * t, -- Вираз
де
  -- Все те, що
  -- використовується
  -- у виразі.
  S = відстань,
  V = швидкість,
  t = час.
```

На відміну від `let`, яке може бути використане для введення супер-локального виразу (як в останньому прикладі з `delta`), `where`-вирази доступні в будь-якій частині виразу, що передує ключовому слову `where`.

## Разом

Ми можемо використовувати `let-in` і `where` спільно, в рамках однієї функції:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40 in
  if | timeInS < threshold -> timeInS + correction
     | timeInS >= threshold -> timeInS + delta + correction
  where
    correction = 120
    delta = 8
```

Частина проміжних значень задана вгорі, а частина &mdash; внизу. Загальна рекомендація: не змішуйте `let-in` і `where` без особливої потреби, такий код читається важко, виглядає надлишковим.

Зазначу, що в якості проміжних можуть виступати і більш складні вирази. Наприклад:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40 in
  if | timeInS < threshold -> timeInS + correction
     | timeInS >= threshold -> timeInS + delta + correction
  where
    -- Цей проміжний вираз залежить від аргументу...
    correction = timeInS * 2
    -- А цей - від іншого виразу...
    delta = correction - 4
```

Вираз `correction` дорівнює `timeInS * 2`, тобто тепер він залежить від значення аргументу функції. А вираз `delta` залежить у свою чергу від `correction`. Причому ми можемо змінювати порядок задання виразів:

```haskell
...
let threshold = 40
in if | timeInS < threshold -> timeInS + correction
      | timeInS >= threshold -> timeInS + delta + correction
where
  delta = correction - 4
  correction = timeInS * 2
```

Вираз `delta` тепер задано першим, але це не має ніякого значення. Адже ми лише оголошуємо рівності, і результат цих оголошень не впливає на кінцевий результат обчислень. Звісно, порядок оголошення рівностей не важливий і для `let`-виразів:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let delta = correction - 4
      threshold = 40
  in if | timeInS < threshold -> timeInS + correction
        | timeInS >= threshold -> timeInS + delta + correction
  where
    correction = timeInS * 2
```

Мало того, що ми поставили `let`-вирази в іншому порядку, так ми ще й використовували в одному з них вираз `correction`! Тобто в `let`-виразі використовувався `where`-вираз. А ось виконати зворотне, на жаль, не вийде:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let delta = correction - 4
     threshold = 40
  in
  if | timeInS < threshold -> timeInS + correction
     | timeInS >= threshold -> timeInS + delta + correction
  where
    correction = timeInS * 2 * threshold -- З let??
```

При спробі скомпілювати такий код ми отримаємо помилку:

```bash
Not in scope: 'threshold'
```

Обмеження таке: використовувати `let`-вирази всередині `where`-виразів неможливо, адже останні вже не входять у вираз, що йде за словом `in`.

Ну що ж, пора рухатися далі, адже вміст наших функцій не обмежений умовними конструкціями.
