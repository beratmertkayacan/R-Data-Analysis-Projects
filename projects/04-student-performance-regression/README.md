# Öğrenci Başarısı Regresyon Analizi

Elli öğrencilik bir veri seti üzerinde veri temizliğinden model tanısına uzanan uçtan uca regresyon çalışması.

**Veri:** Tekrarlanabilirlik için sabit tohumla kod içinde üretiliyor, ayrı bir dosya gerekmiyor
**Kaynak:** [`regression_analysis.Rmd`](regression_analysis.Rmd)
**Rapor:** [`regression_analysis.md`](regression_analysis.md)

## Akış

Veri seti bilinçli olarak kirli üretiliyor. İçinde eksik değerler, farklı biçimlerde yazılmış kategoriler ve mantık dışı gözlemler var. Çalışma sırasıyla şu adımları izliyor.

1. Eksik değerler medyan ile dolduruluyor, medyan aykırı değerlerden etkilenmediği için tercih ediliyor
2. Kategorik sütundaki yazım farklılıkları temizlenip iki seviyeli faktöre çevriliyor
3. Mantık dışı değerler winsorization ile sınıra çekiliyor, böylece gözlem kaybı olmuyor
4. Dağılımlar histogram ve kutu grafiğiyle inceleniyor
5. Pearson ve Spearman katsayıları karşılaştırılıyor
6. Çoklu doğrusal regresyon modeli kuruluyor ve yorumlanıyor
7. Artık analizi ve Cook mesafesi ile model varsayımları sınanıyor

## Bulgular

| Ölçüt | Değer |
|---|:---:|
| R kare | 0.9686 |
| RMSE | 2.66 |
| Pearson r | 0.828 |
| Spearman r | 0.872 |

Çalışma saati ve devam oranı modelde anlamlı çıkıyor, ders dışı etkinlik katılımı ise 0.45 p değeriyle anlamsız kalıyor. Pearson ve Spearman katsayılarının birbirine yakın olması ilişkinin büyük ölçüde doğrusal olduğunu gösteriyor.

Cook mesafesi grafiğinde 3, 4 ve 13 numaralı gözlemler eşiği aşıyor. En belirgini 13 numaralı gözlem, yani notu 250 olarak girilip temizlik aşamasında 100 e çekilen kayıt. Winsorization bu gözlemin model katsayıları üzerindeki etkisini sınırlıyor.

## Çalıştırma

```r
rmarkdown::render("projects/04-student-performance-regression/regression_analysis.Rmd")
```
