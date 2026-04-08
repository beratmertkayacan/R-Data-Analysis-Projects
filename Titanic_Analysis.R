#Data Understanding
data <- read.csv("titanic.csv", header = TRUE, na.strings = "?") #oku, başlığı atla, ? varsa na ver
head(titanic) #ilk 6 satırı yazdır
dim(titanic) #boyutunu göster
names(titanic) # sutün isimlerini listele
sapply(titanic, function(x) length(unique(x))) # her sutündaki farklı değer sayısını hesapla(0 ve 1-0,1,2..)
table(titanic$Survived) # kaç kişi hayatta 

#Data Cleaning

# NA olan yerleri temizle
colSums(is.na(titanic)) # na ları göster eksikler neler
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE) # yaş sutünundaki eksiklere ortalamayı yaz
titanic$Fare[is.na(titanic$Fare)] <- mean(titanic$Fare, na.rm = TRUE) # fare sutünundaki eksiklere ortalamayı yaz
colSums(is.na(titanic)) #eksik var mı kontrol et

cabin_mode <- names(sort(table(titanic$Cabin), decreasing = TRUE))# en sık tekrar eden cabin no bul
titanic$Cabin[is.na(titanic$Cabin)] <- cabin_mode # na olan yerlere bu no'yu yaz
colSums(is.na(titanic)) #eksik var mı kontrol et
# NA olan yerler temizlendi


#Visualization

#histogram ile yolcu yaş dağılımı 
hist(titanic$Age, breaks = 30, freq = FALSE, 
     col = "#4C72B0", border = "white",
     main = "Yolcu Yaş Dağılımı", xlab = "Yaş", ylab = "Yoğunluk")
lines(density(titanic$Age), col = "#DD8452", lwd = 2) #density curve ekle.


#barplot ile hayatta kalma oranı
barplot(table(titanic$Survived), 
        col = c("#C44E52", "#55A868"), 
        names.arg = c("Ölenler (0)", "Kurtulanlar (1)"),
        main = "Hayatta Kalma Sayıları",
        xlab = "Durum", ylab = "Kişi Sayısı")


counts <- table(titanic$Survived, titanic$Sex) #Survived ve Sex tablosunu oluştur
#barplot ile grafiği oluştur
barplot(counts, 
        beside = TRUE, 
        col = c("#C44E52", "#55A868"),
        legend = c("Ölen", "Kurtulan"),
        main = "Cinsiyete Göre Hayatta Kalma",
        xlab = "Cinsiyet", ylab = "Kişi Sayısı")


counts_pclass <- table(titanic$Survived, titanic$Pclass) #Pclass ve Survived tablosunu oluştur.
#barplot ile grafiği oluştur
barplot(counts_pclass, 
        beside = TRUE, 
        col = c("#C44E52", "#55A868"), 
        legend = c("Ölen", "Kurtulan"), 
        args.legend = list(x = "top"),
        main = "Survival by Passenger Class",
        xlab = "Passenger Class (1=Üst, 2=Orta, 3=Alt)", 
        ylab = "Count")


stripchart(Survived ~ Sex, data = titanic, #Jitter kullanarak cinsiyete göre survived 
           method = "jitter", jitter = 0.15, 
           pch = 19, col = c("#DD8452", "#4C72B0"),
           main = "Survival Distribution by Sex")

