# Doğrusal Regresyon

Advertising ve Auto veri setleri üzerinde basit ve çoklu doğrusal regresyon uygulaması.

**Veri:** [`data/advertising.csv`](../../data/advertising.csv), [`data/auto.csv`](../../data/auto.csv)
**Kaynak:** [`linear_regression.Rmd`](linear_regression.Rmd)
**Rapor:** [`linear_regression.md`](linear_regression.md)

## Bulgular

| Model | Açıklayıcılar | R kare | Artık standart hatası |
|---|---|:---:|:---:|
| Basit | TV | 0.612 | 3.259 |
| Çoklu | TV, radyo, gazete | 0.897 | 1.686 |
| Auto | yedi teknik değişken | 0.822 | 3.328 |

Çoklu modelde TV ve radyo harcamaları güçlü şekilde anlamlı çıkarken gazete harcamasının katsayısı 0.86 p değeriyle anlamsız kalıyor. Auto modelinde ağırlık, model yılı, menşe ve motor hacmi anlamlı, silindir sayısı ile ivmelenme anlamsız.

Basit modelin artık grafikleri iki noktaya işaret ediyor. Artıkların yayılımı tahmin değeri büyüdükçe belirgin biçimde genişliyor, yani sabit varyans varsayımı sağlanmıyor. Düzleştirilmiş eğri de hafif bir kavis çizdiği için TV harcaması ile satış arasındaki ilişki tam olarak doğrusal değil.

## Çalıştırma

```r
rmarkdown::render("projects/02-linear-regression/linear_regression.Rmd")
```
