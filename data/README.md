# Veri Setleri

Depodaki tüm projeler veriyi bu klasörden okur. Proje klasörleri içinden erişim `../../data/` yoluyla yapılır.

## advertising.csv

200 pazar gözleminde üç mecraya yapılan reklam harcaması ve elde edilen satış. Kaynak: An Introduction to Statistical Learning.

| Sütun | Tip | Açıklama |
|---|---|---|
| `TV` | sayısal | TV reklam harcaması, bin dolar |
| `radio` | sayısal | Radyo reklam harcaması, bin dolar |
| `newspaper` | sayısal | Gazete reklam harcaması, bin dolar |
| `sales` | sayısal | Satış, bin adet |

## auto.csv

392 otomobil modeline ait teknik özellikler ve yakıt tüketimi. Eksik beygir gücü değerleri soru işareti ile kodlandığı için `na.strings = "?"` ile okunur.

| Sütun | Tip | Açıklama |
|---|---|---|
| `mpg` | sayısal | Galon başına mil, yakıt verimliliği |
| `cylinders` | sayısal | Silindir sayısı |
| `displacement` | sayısal | Motor hacmi, inç küp |
| `horsepower` | sayısal | Beygir gücü, 5 eksik değer içerir |
| `weight` | sayısal | Ağırlık, libre |
| `acceleration` | sayısal | 0 dan 60 mil hıza çıkış süresi, saniye |
| `year` | sayısal | Model yılı, son iki hane |
| `origin` | sayısal | Menşe (1 Amerika, 2 Avrupa, 3 Japonya) |
| `name` | metin | Model adı |

## default.csv

10000 müşteri için kredi kartı ödememe kaydı. ISLR paketindeki `Default` veri setinin dışa aktarılmış kopyasıdır. Lojistik regresyon projesi veriyi doğrudan ISLR paketinden yükler, bu dosya paket kurulu olmadan incelemek isteyenler için tutulur.

| Sütun | Tip | Açıklama |
|---|---|---|
| `default` | kategorik | Ödememe durumu (Yes, No) |
| `student` | kategorik | Öğrenci olup olmadığı (Yes, No) |
| `balance` | sayısal | Ortalama kalan kart borcu |
| `income` | sayısal | Yıllık gelir |

## titanic.csv

418 yolcu kaydı. Kaggle Titanic yarışmasının etiketlenmiş test bölümüdür. Yolcuların 152 si hayatta kalmış, 266 sı hayatını kaybetmiştir.

| Sütun | Tip | Açıklama |
|---|---|---|
| `PassengerId` | sayısal | Yolcu numarası |
| `Survived` | sayısal | Hayatta kalma (0 hayır, 1 evet) |
| `Pclass` | sayısal | Bilet sınıfı (1 üst, 2 orta, 3 alt) |
| `Name` | metin | Yolcu adı |
| `Sex` | kategorik | Cinsiyet |
| `Age` | sayısal | Yaş, 86 eksik değer |
| `SibSp` | sayısal | Gemideki kardeş ve eş sayısı |
| `Parch` | sayısal | Gemideki ebeveyn ve çocuk sayısı |
| `Ticket` | metin | Bilet numarası |
| `Fare` | sayısal | Bilet ücreti, 1 eksik değer |
| `Cabin` | metin | Kabin numarası, 327 kayıtta boş |
| `Embarked` | kategorik | Biniş limanı (C Cherbourg, Q Queenstown, S Southampton) |
