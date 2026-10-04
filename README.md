# R ile Veri Analizi Projeleri

Bahçeşehir Üniversitesi veri bilimi çalışmaları kapsamında R ile yürüttüğüm keşifsel veri analizi ve regresyon projelerinin toplandığı depo. Her proje kendi klasöründe duruyor, veri setleri ortak bir `data` klasöründe tutuluyor ve R Markdown ile üretilen raporlar grafikleriyle birlikte GitHub üzerinde doğrudan okunabiliyor.

**Kullanılan araçlar:** R, R Markdown, knitr, temel R grafikleri, ISLR, caTools, RStudio

## Projeler

| # | Proje | Konu | Veri | Çıktı |
|---|---|---|---|---|
| 01 | [Titanic Keşifsel Veri Analizi](projects/01-titanic-eda) | Veri temizliği ve görselleştirme | `titanic.csv` | R betiği |
| 02 | [Doğrusal Regresyon](projects/02-linear-regression) | Basit ve çoklu doğrusal regresyon, artık analizi | `advertising.csv`, `auto.csv` | [Rapor](projects/02-linear-regression/linear_regression.md) |
| 03 | [Lojistik Regresyon](projects/03-logistic-regression) | Sınıflandırma, eğitim ve test ayrımı, karışıklık matrisi | ISLR `Default` | [Rapor](projects/03-logistic-regression/logistic_regression.md) |

### 01. Titanic Keşifsel Veri Analizi

418 yolcu kaydı üzerinde eksik veri temizliği ve görselleştirme. Age ve Fare sütunlarındaki eksik değerler ortalama ile dolduruluyor, yolcuların büyük bölümünde bulunmayan kabin bilgisi ayrı bir kategori olarak işaretleniyor. Yaş dağılımı, hayatta kalma sayıları, cinsiyet ve yolcu sınıfı kırılımları temel R grafikleriyle inceleniyor.

### 02. Doğrusal Regresyon

Advertising veri setinde önce yalnızca TV harcamasıyla basit bir model kuruluyor, ardından radyo ve gazete harcamaları eklenerek çoklu modele geçiliyor.

| Model | Açıklayıcılar | R kare | Not |
|---|---|:---:|---|
| Basit | TV | 0.612 | TV katsayısı anlamlı |
| Çoklu | TV, radyo, gazete | 0.897 | Gazete harcaması anlamsız (p = 0.86) |

Gazete harcaması modele katkı sağlamıyor, satıştaki değişimi açıklayan asıl kalemler TV ve radyo. Auto veri setinde ise yakıt tüketimi yedi değişkenle modellenip 0.822 R kare elde ediliyor, ağırlık, model yılı ve menşe en güçlü açıklayıcılar olarak öne çıkıyor.

### 03. Lojistik Regresyon

ISLR paketindeki Default veri setinde kredi ödememe durumu hesap bakiyesi üzerinden modelleniyor. Veri, `sample.split` ile 8000 eğitim ve 2000 test satırına ayrılıyor, 0.5 eşiğiyle sınıflandırma yapılıyor.

| Ölçüt | Değer |
|---|:---:|
| Doğruluk | 0.9735 |
| Doğru sınıflanan ödememe | 25 |
| Kaçırılan ödememe | 42 |

Genel doğruluk yüksek görünse de modelin kaçırdığı ödememe sayısı yakaladığından fazla. Bu, sınıf dengesizliği olan veri setlerinde doğruluk ölçütünün tek başına yeterli olmadığını gösteriyor.

## Depo Yapısı

```
.
├── README.md
├── R-Data-Analysis-Projects.Rproj
├── data/
│   ├── README.md
│   ├── advertising.csv
│   ├── auto.csv
│   ├── default.csv
│   └── titanic.csv
└── projects/
    ├── 01-titanic-eda/
    │   ├── README.md
    │   └── titanic_eda.R
    ├── 02-linear-regression/
    │   ├── README.md
    │   ├── linear_regression.Rmd
    │   ├── linear_regression.md
    │   └── linear_regression_files/
    └── 03-logistic-regression/
        ├── README.md
        ├── logistic_regression.Rmd
        ├── logistic_regression.md
        └── logistic_regression_files/
```

Her proje klasörü bağımsız çalışır ve veriyi `../../data` yolundan okur. R Markdown dosyaları `github_document` biçiminde derlendiği için üretilen `.md` raporları GitHub üzerinde grafikleriyle birlikte görüntülenir.

## Kurulum ve Çalıştırma

1. R ve RStudio kurulu olmalı. Depo kökündeki `R-Data-Analysis-Projects.Rproj` dosyası RStudio ile açıldığında çalışma dizini doğru şekilde ayarlanır.
2. Gerekli paketleri kurun:

```r
install.packages(c("ISLR", "caTools", "rmarkdown", "knitr"))
```

3. R betiğini çalıştırmak için `projects/01-titanic-eda/titanic_eda.R` dosyasını açıp çalıştırın.
4. Raporları yeniden üretmek için ilgili `.Rmd` dosyasını açıp RStudio içinden Knit edin ya da konsoldan derleyin:

```r
rmarkdown::render("projects/02-linear-regression/linear_regression.Rmd")
rmarkdown::render("projects/03-logistic-regression/logistic_regression.Rmd")
```

Veri setlerinin tanımı ve sütun açıklamaları için [data/README.md](data/README.md) dosyasına bakın.

## English Summary

This repository collects the exploratory data analysis and regression work I carried out in R. Each study lives in its own project folder, all data sets sit in a shared `data` directory, and the R Markdown reports are rendered as GitHub documents so they can be read with their figures directly on GitHub.

The Titanic project covers missing value handling and base R visualization over 418 passenger records. The linear regression project fits simple and multiple models on the Advertising data, where adding radio spend raises R squared from 0.612 to 0.897 and newspaper spend turns out to be insignificant, and then models fuel consumption on the Auto data with an R squared of 0.822. The logistic regression project predicts credit default from account balance on the ISLR Default data, reaching 0.9735 accuracy on a held out test set while still missing more defaults than it catches, which illustrates why accuracy alone is misleading under class imbalance.
