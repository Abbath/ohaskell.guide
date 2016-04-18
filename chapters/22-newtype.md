# Новий тип

Крім `data` існує ще одне ключове слово, призначене для визначення нового типу. Воно так і називається &mdash; `newtype`. Ці слова схожі одне на одне &laquo;в односторонньому порядку &raquo;: ви можете поставити `data` на місце `newtype`, але не навпаки.

## Відмінності

Тип, який визначається за допомогою слова `newtype`, зобов'язаний мати один і тільки один конструктор значення. Ми можемо написати так:

```haskell
newtype IPAddress = IP String
```

А ось так не можемо:

```haskell
newtype IPAddress = IP String | Localhost
```

Компілятор буде проти:

```bash
A newtype must have exactly one constructor,
  but 'IPAddress' has two
In the newtype declaration for 'IPAddress'
```

Окрім того, в такому типі має бути одне і тільки одне поле. Тобто можна так:

```haskell
newtype IPAddress = IP String
```

Або так, з міткою:

```haskell
newtype IPAddress = IP { value :: String }
```

А ось два або більше полів запхати не вдасться:

```haskell
newtype EndPoint = EndPoint String Int
```

Компілятор знову зверне нашу увагу на проблему:

```bash
The constructor of a newtype must have exactly one field
  but 'Security' has two
In the definition of data constructor 'Security'
In the newtype declaration for 'Security'
```

Ба навіть більше, нульарный конструктор теж не підійде:

```haskell
newtype HardDay = Monday
```

І знову помилка:

```bash
The constructor of a newtype must have exactly one field
  but 'Monday' has none
```

## Навіщо він потрібен?

Справді, навіщо нам потрібна така річ? Це не можна, те не можна. В чому сенс?

Сенс в оптимізації. Зверніть увагу на модель `newtype`:

```haskell
newtype IPAddress = IP          String

новий   назва       конструктор поле
тип                 значення
```

Фактично, `newtype` бере одне значення певного існуючого типу та лише загортає його у свій конструктор. Саме тому тип, введений за допомогою `newtype`, не відноситься до АТД, та з точки зору компілятора він є лише перейменуванням типу (англ. type renaming). Це робить такий тип більш простим та ефективним з точки зору представлення в пам'яті, аніж тип, який визначається за допомогою `data`.

Коли ми пишемо так:

```haskell
data IPAddress = IP String
```

ми кажемо компілятору: &laquo;`IPAddress` &mdash; це абсолютно новий та самобутній тип, якого ніколи не було раніше&raquo;. А коли пишемо так:

```haskell
newtype IPAddress = IP String
```

ми кажемо: &laquo;`IPAddress` &mdash; це лише обгортка для значення вже існуючого типу `String`&raquo;.

Ось ми і познайомилися з `newtype`-типами. У багатьох пакетах ви не раз зустрінете такі типи.

## Для допитливих

Уважний читач запитає, в чому ж фундаментальна відмінність типів, які створюються за допомогою `newtype`, від типів, що вводяться з допомогою `type`? Там синонім, тут &mdash; обгортка. Відмінність ось у чому.

Коли ми пишемо так:

```haskell
type String = [Char]
```

ми оголошуємо: &laquo;Тип `String` &mdash; це еквівалентна заміна типу `[Char]`&raquo;. Тому скрізь, де в коді стоїть `[Char]`, ми можемо поставити `String`, а всюди, де стоїть `String`, ми можемо поставити `[Char]`. Наприклад, якщо функція оголошена так:

```haskell
replace :: String
        -> String
        -> String
        -> String
```

ми можемо спокійно переписати оголошення:

```haskell
replace :: [Char]
        -> [Char]
        -> [Char]
        -> [Char]
```

і нічого не зміниться.

Якщо ж ми пишемо так:

```haskell
newtype MyInt = MyInt Int
```

ми оголошуємо: &laquo;Тип `MyInt` &mdash; це новий тип, представлення якого таке ж, як у типу `Int`&raquo;. Ми не можемо просто взяти й поставити `MyInt` на місце `Int`, тому що ці типи рівні лише з точки зору представлення в пам'яті, а з точки зору системи типів вони абсолютно різні.