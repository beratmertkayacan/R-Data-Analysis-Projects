# Lojistik Regresyon

ISLR paketindeki Default veri setinde kredi kartı ödememe durumunun hesap bakiyesi üzerinden tahmin edilmesi.

**Veri:** ISLR paketindeki `Default`, dışa aktarılmış kopyası [`data/default.csv`](../../data/default.csv)
**Kaynak:** [`logistic_regression.Rmd`](logistic_regression.Rmd)
**Rapor:** [`logistic_regression.md`](logistic_regression.md)

## Yöntem

Veri `sample.split` ile hedef değişkene göre tabakalı biçimde 8000 eğitim ve 2000 test satırına ayrılıyor. Tekrarlanabilirlik için tohum 42 olarak sabitleniyor. Model, ödememe olasılığını hesap bakiyesinin fonksiyonu olarak tahmin ediyor ve 0.5 eşiğiyle sınıflandırma yapılıyor.

## Sonuçlar

Test kümesindeki karışıklık matrisi:

| | Gerçek No | Gerçek Yes |
|---|:---:|:---:|
| **Tahmin No** | 1922 | 42 |
| **Tahmin Yes** | 11 | 25 |

Doğruluk 0.9735. Buna karşılık model, gerçekte ödememe yapan 67 müşterinin yalnızca 25 ini yakalıyor. Veri setinde ödememe oranı yüzde 3.3 olduğu için her müşteriye No demek bile yaklaşık 0.967 doğruluk verir. Bu nedenle bu tür dengesiz veride doğruluk yerine duyarlılık ve kesinlik ölçütlerine bakmak gerekir.

## Çalıştırma

```r
rmarkdown::render("projects/03-logistic-regression/logistic_regression.Rmd")
```
