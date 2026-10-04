# Titanic Keşifsel Veri Analizi
# Veri: ../../data/titanic.csv (418 yolcu, Survived etiketi dahil)

# ---- Veriyi tanıma ----------------------------------------------------------

# Boş hücreler ve soru işaretleri eksik değer olarak okunur
titanic <- read.csv("../../data/titanic.csv", header = TRUE,
                    na.strings = c("", "?", "NA"))

head(titanic) # ilk altı satır
dim(titanic) # satır ve sütun sayısı
names(titanic) # sütun isimleri
sapply(titanic, function(x) length(unique(x))) # her sütundaki farklı değer sayısı
table(titanic$Survived) # hayatta kalan ve kalmayan sayısı

# ---- Veri temizliği ---------------------------------------------------------

colSums(is.na(titanic)) # hangi sütunda kaç eksik değer var

# Age ve Fare sayısal olduğu için ortalama ile dolduruluyor
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)
titanic$Fare[is.na(titanic$Fare)] <- mean(titanic$Fare, na.rm = TRUE)

# Kabin bilgisi yolcuların büyük bölümünde yok. Bu kadar yüksek eksiklikte
# en sık kabini atamak yanıltıcı olur, bu yüzden ayrı bir kategori veriliyor.
titanic$Cabin[is.na(titanic$Cabin)] <- "Unknown"

colSums(is.na(titanic)) # temizlik sonrası kontrol

# ---- Görselleştirme ---------------------------------------------------------

# Yolcu yaş dağılımı ve yoğunluk eğrisi
hist(titanic$Age, breaks = 30, freq = FALSE,
     col = "#4C72B0", border = "white",
     main = "Yolcu Yaş Dağılımı", xlab = "Yaş", ylab = "Yoğunluk")
lines(density(titanic$Age), col = "#DD8452", lwd = 2)

# Hayatta kalma sayıları
barplot(table(titanic$Survived),
        col = c("#C44E52", "#55A868"),
        names.arg = c("Ölenler (0)", "Kurtulanlar (1)"),
        main = "Hayatta Kalma Sayıları",
        xlab = "Durum", ylab = "Kişi Sayısı")

# Cinsiyete göre hayatta kalma
counts_sex <- table(titanic$Survived, titanic$Sex)
barplot(counts_sex,
        beside = TRUE,
        col = c("#C44E52", "#55A868"),
        legend = c("Ölen", "Kurtulan"),
        main = "Cinsiyete Göre Hayatta Kalma",
        xlab = "Cinsiyet", ylab = "Kişi Sayısı")

# Yolcu sınıfına göre hayatta kalma
counts_pclass <- table(titanic$Survived, titanic$Pclass)
barplot(counts_pclass,
        beside = TRUE,
        col = c("#C44E52", "#55A868"),
        legend = c("Ölen", "Kurtulan"),
        args.legend = list(x = "top"),
        main = "Yolcu Sınıfına Göre Hayatta Kalma",
        xlab = "Yolcu Sınıfı (1 üst, 2 orta, 3 alt)",
        ylab = "Kişi Sayısı")

# Cinsiyete göre hayatta kalma dağılımı
stripchart(Survived ~ Sex, data = titanic,
           method = "jitter", jitter = 0.15,
           pch = 19, col = c("#DD8452", "#4C72B0"),
           main = "Cinsiyete Göre Hayatta Kalma Dağılımı")
