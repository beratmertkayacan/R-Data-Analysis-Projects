# Titanic Keşifsel Veri Analizi

Titanic yolcu verisi üzerinde eksik değer temizliği ve temel R grafikleriyle görselleştirme çalışması.

**Veri:** [`data/titanic.csv`](../../data/titanic.csv), 418 yolcu kaydı
**Betik:** [`titanic_eda.R`](titanic_eda.R)

## Yapılan işlemler

1. Veri setinin boyutu, sütunları ve her sütundaki farklı değer sayısı inceleniyor.
2. Eksik değerler çıkarılıyor. Age sütununda 86, Fare sütununda 1, Cabin sütununda 327 kayıt eksik.
3. Age ve Fare eksikleri sütun ortalamasıyla dolduruluyor.
4. Kabin bilgisi kayıtların dörtte üçünde bulunmadığı için en sık değerle doldurmak yerine Unknown kategorisine alınıyor.
5. Dört grafikle dağılımlar inceleniyor: yaş histogramı ve yoğunluk eğrisi, hayatta kalma sayıları, cinsiyete göre kırılım, yolcu sınıfına göre kırılım.

## Çalıştırma

```r
source("projects/01-titanic-eda/titanic_eda.R")
```

Betik veriyi `../../data/titanic.csv` yolundan okur, bu yüzden RStudio içinde dosya açıkken çalıştırmak yeterlidir.
