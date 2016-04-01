# АТД: поля з мітками

Багато типів в реальних проектах досить великі. Погляньте:

```haskell
data Arguments = Arguments Port
                           Endpoint
                           RedirectData
                           FilePath
                           FilePath
                           Bool
                           FilePath
```

Значення типу `Arguments` зберігає в своїх полях деякі значення, отримані з параметрів командного рядка, з якими запущена одна з моїх програм. І все було б добре, але працювати з таким типом абсолютно незручно. Він містить сім полів, і патерн матчінг був би занадто громіздким, уявіть собі:

```haskell
...
where
Arguments _ _ _ redirectLib _ _ xpi = arguments
```

Більше того, коли ми дивимося на визначення типу, призначення полів залишається таємницею за сімома печатями. Бачите передостаннє поле? Воно має тип `Bool` і, ясна річ, відображає якийсь прапорець. Але що це за прапорець, читач не представляє. На щастя, існує спосіб, який рятує нас від обох цих проблем.

## Мітки

Ми можемо забезпечити наші поля мітками (англ. label). Ось як це виглядає:

```haskell
data Arguments = Arguments { runWDServer :: Port
                           , withWDServer :: Endpoint
                           , redirect :: RedirectData
                           , redirectLib :: FilePath
                           , screenshotsDir :: FilePath
                           , noScreenshots :: Bool
                           , harWithXPI :: FilePath
                           }
```

Тепер призначення міток більш зрозуміле. Схема визначення така:

```haskell
data Arguments = Arguments   { runWDServer :: Port }

тип  такий-то    конструктор   мітка поля     тип
                                              поля
```

Тепер поле має не тільки тип, але і назву, що робить наше визначення значно більш чительним. Поля в цьому випадку розділені комами і знаходяться в фігурних дужках.

Якщо два чи більше полів одного типу йдуть поспіль, тип можна вказати лише для останньої з міток. Так, якщо у нас є ось такий тип:

```haskell
data Patient = Patient { firstName :: String
                       , lastName :: String
                       , email :: String
                       }
```

його визначення можна трохи спростити і написати так:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email :: String
                       }
```

Оскільки тип всіх трьох полів однаковий, ми вказуємо його лише для останньої з міток. Ще приклад повної форми:

```haskell
data Patient = Patient { firstName :: String
                       , lastName :: String
                       , email :: String
                       , age :: Int
                       , diseaseId :: Int
                       , isIndoor :: Bool
                       , hasInsurance :: Bool
                       }
```

і тут же спрощуємо:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email :: String
                       , age
                       , diseaseId :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }
```

Поля `firstName`, `lastName` і `email` мають тип `String`, поля `age` і `diseaseId` &mdash; тип `Int`, і два поля &mdash; тип `Bool`.

## Getter і Setter?

Що ж таке мітки? Фактично, це особливі функції, згенеровані автоматично. Ці функції мають три призначення: створювати, вилучати і змінювати. Так, я не обмовився, змінювати. Але про це трохи пізніше, нехай буде маленька інтрига.

Ось як ми створюємо значення типу `Patient`

```haskell
main :: IO ()
main = print $ diseaseId patient
  where
  patient = Patient {
    firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@gmail.com"
    , age = 24
    , diseaseId = 431
    , isIndoor = True
    , hasInsurance = True
    }
```

Мітки полів використовуються як свого роду setter (від англ. set, &laquo;встановлювати&raquo;):

```haskell
patient = Patient { firstName    =      "John"
у цьому   типу      поле з
значення  Patient   цією міткою  рівне  цьому рядку
```

Крім того, мітку можна використовувати і як getter (від англ. get, &laquo;отримувати&raquo;):

```haskell
main = print $ diseaseId   patient

               мітка як    аргумент
               функція
```

Ми застосовуємо мітку до значення типу `Patient` і отримуємо значення відповідного цій мітці поля. Тому для отримання значень полів нам уже не потрібен патерн матчінг.

Але що ж за інтригу я приготував під кінець? Вище я згадав, що мітки використовуються не тільки для задання значень полів і для їх отримання, але і для зміни. Ось що я мав на увазі:

```haskell
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com" -- Змінюємо???
    }

    patient = Patient {
      firstName = "John"
      , lastName = "Doe"
      , email = "john.doe@gmail.com"
      , age = 24
      , diseaseId = 431
      , isIndoor = True
      , hasInsurance = True
    }
```

При запуску програми отримаємо:

```haskell
j.d@gmail.com
```

Але постривайте, що ж тут сталося? Адже в Haskell, як ми знаємо, немає оператора присвоювання, однак значення поля з міткою `email` змінилося. Пам'ятаю, коли я вперше побачив подібний приклад, то дуже здивувався, мовляв, чи не ввели мене в оману щодо незмінності значень в Haskell?!

Ні, не ввели. Подібний запис:

```haskell
patientWithChangedEmail = patient {
  email = "j.d@gmail.com"
}
```

дійсно схожий на зміну поля через присвоєння йому нового значення, але в дійсності ніякої зміни не відбулося. Коли я назвав мітку setter-ом, я трохи злукавив, адже класичний setter зі світу ООП був би неможливий в Haskell. Подивимося ще раз уважніше:

```haskell
...
where
  patientWithChangedEmail = patient {
    email = "j.d@gmail.com" -- Змінюємо???
  }

  patient = Patient {
    firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@gmail.com"
    , age = 24
    , diseaseId = 431
    , isIndoor = True
    , hasInsurance = True
  }
```

Погляньте, адже у нас тепер два значення типу `Patient`, `patient` і `patientWithChangedEmail`. Ці значення не мають одне до одної жодного стосунку. Згадайте, як я говорив, що в Haskell не можна змінити наявні значення, а можна лише створити на основі наявного нове значення. Це саме те, що тут сталося: ми взяли наявне значення `patient` і на його основі створили вже нове значення `patientWithChangedEmail`, значення поля `email` в якому тепер інше. Зрозуміло, що поле `email` у значенні `patient` залишилося незмінним.

Будьте уважні при ініціалізації значення з полями: ви зобов'язані надати значення для всіх полів. Якщо ви напишете так:

```haskell
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com" -- Змінюємо???
    }

    patient = Patient {
      firstName = "John"
      , lastName = "Doe"
      , email = "john.doe@gmail.com"
      , age = 24
      , diseaseId = 431
      , isIndoor = True
    }

    -- Поле hasInsurance забули!
```

код зкомпілюється, але уважний компілятор попередить вас про проблему:

```haskell
Fields of 'Patient' not initialised: hasInsurance
```

Будь ласка, не нехтуйте подібним попередженням, адже якщо ви проігноруєте його і потім спробуєте звернутися до неініціалізованого поля:

```haskell
main = print $ hasInsurance patient
...
```

ваша програма аварійно завершиться на етапі виконання з очікуваною помилкою:

```bash
Missing record field in construction hasInsurance
```

Не забувайте: компілятор &mdash; ваш добрий друг.

## Без міток

Пам'ятайте, що мітки полів &mdash; це синтаксичний цукор (англ. syntactic sugar), і ми можемо обійтися без нього. Навіть якщо тип був визначений з мітками, як наш `Patient`, ми можемо працювати з ним по-старому:

```haskell
data Patient = Patient { firstName :: String
                         , lastName :: String
                         , email :: String
                         , age :: Int
                         , diseaseId :: Int
                         , isIndoor :: Bool
                         , hasInsurance :: Bool
                         }

main :: IO ()
main = print $ hasInsurance patient
  where
    -- Створюємо по-старому...
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      True
                      True
```

Відповідно, отримувати значення полів теж можна по-старому, через патерн матчінг:

```haskell
main :: IO ()
main = print insurance
  where
    -- Страшенно незручно, але якщо бажаєте...
    Patient _ _ _ _ _ _ insurance = patient
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      True
                      True
```

З поняттям &laquo;синтаксичний цукор&raquo; ми зустрінемося ще не раз, на куди більш просунутих прикладах.
