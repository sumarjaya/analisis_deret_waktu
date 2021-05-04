setwd("D:/my-campus/catatan-kuliah/analisis-deret-waktu/sampel-analysis/co2")
getwd()
ls()
rm(list=ls(all=TRUE))
ls(all=TRUE)



### Sumber data: https://www.esrl.noaa.gov/gmd/ccgg/trends/data.html

#1. Memasukkan data ke R
co2.baru = read.csv("co2_mm_mlo.csv",header=TRUE) # header dipakai
head(co2.baru) # menampilkan 6 data pertama
co2.baru = co2.baru$average # mengambil data rata-rata bulanan
co2.baru

#2. Mengubah data menjadi tipe deret waktu
co2.baru = ts(co2.baru,start=c(1958,3),frequency=12) # mengubah data
# co2 menjadi tipe deret waktu bulanan
co2.baru
start(co2.baru) # awal deret waktu co2
end(co2.baru) # akhir deret waktu co2
length(co2.baru) # banyak data

#3. Memplot data
plot(co2.baru)
# Observasi: ada tren dan fluktuasi musiman
# Pada tahap ini kita menyimpulkan data tidak stasioner karena
# ada tren.

#4. Melihat ACF data (melihat seberapa kuat korelasi dalam data)
par(mfrow=c(2,1))
acf(as.vector(co2.baru),lag=50)
acf(as.vector(co2.baru),type="partial",lag=50)
# Kita bisa menggunakan fungsi acf2 pada library(astsa)
library(astsa)
acf2(as.vector(co2.baru))
## 

#5. Uji formal kestasioneran menggunakan alfa = 0,05 (default)
library(tseries)
adf.test(co2.baru) # hipotesis nol data tidak stasioner: diterima
# kpss.test(co2.baru,null="Trend")
# pp.test(co2.baru) # hipotesi nol: data tidak stasioner

# Kesimpulan: berdasarkan uji formal kestasioneran menggunakan
# uji ADF, data tidak stasioner

#6. Melakukan differencing terhadap tren
diff.tren.co2.baru = diff(co2.baru,lag=1) # differencing tren, lag = 1
# bisa juga tidak ditulis karena default lag = 1 pada R
plot(diff.tren.co2)
acf2(as.vector(diff.tren.co2))
# ACF memperlihatkan pola musiman yang kuat (berosilasi) seperti
# gelombang sinus-kosinus

#7. Melakukan differencing terhadap musim
diff.musim.co2.baru = diff(diff.tren.co2.baru,lag=12)
plot(diff.musim.co2.baru)
# Untuk melihat perbedaan antara data yang belum didifferencing
# dan yang sudah didifferencing
# plot(co2.baru)
# plot(diff.tren.co2)
# plot(diff.musim.co2.baru)
acf2(as.vector(diff.musim.co2.baru))

#8. Spesifikasi model SARIMA(p,d,q)x(P,D,Q)_{12}
sarima.co2.baru.011.011 = arima(co2.baru,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
sarima.co2.baru.011.111 = arima(co2.baru,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
sarima.co2.baru.110.011 = arima(co2.baru,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12))
sarima.co2.baru.111.011 = arima(co2.baru,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
sarima.co2.baru.210.011 = arima(co2.baru,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))

sarima.co2.baru.011.011$aic
sarima.co2.baru.011.111$aic
sarima.co2.baru.110.011$aic
sarima.co2.baru.111.011$aic # model ini memiliki AIC terkecil di antara kelima kandidat
sarima.co2.baru.210.011$aic

#9. Memilih model yang memiliki AIC paling kecil di antara kandidat
#model yang kita usulkan

sarima.aic = c(sarima.co2.baru.011.011$aic,sarima.co2.baru.011.111$aic,
sarima.co2.baru.110.011$aic,sarima.co2.baru.111.011$aic,sarima.co2.baru.210.011$aic)
sarima.model = c("SARIMA(0,1,1)x(0,1,1)","SARIMA(0,1,1)x(1,1,1)","SARIMA(1,1,0)x(0,1,1)",
"SARIMA(1,1,1)x(0,1,1)","SARIMA(2,1,0)x(0,1,1)")
sarima.seleksi = cbind(sarima.model,sarima.aic)
sarima.seleksi = data.frame(sarima.seleksi)
sarima.seleksi

#10. Melakukan diagnostik model
## uji kenormalan

# plot residual model (tidak membentuk pola yang berisi tren)
res.sarima.co2.baru.111.011 = sarima.co2.baru.111.011$residuals
plot.ts(res.sarima.co2.baru.111.011)
abline(h=0,col="red")

# plot kuantil-kuantil
qqnorm(res.sarima.co2.baru.111.011)
qqline(res.sarima.co2.baru.111.011)

# uji kenormalan
shapiro.test(res.sarima.co2.baru.111.011) # Ho diterima (residual menyebar normal)

# diagnostik juga bisa dilakukan menggunakan fungsi sarima pada 
# library(astsa)
sarima(co2.baru,1,1,1,0,1,1,12)

# uji autokorelasi Ljung-Box memperlihatkan p-value 
# melewati garis putus-putus, artinya tidak ada korelasi
# antarresidual

#11. Peramalan data co2
## Prediksi
forecast.co2.baru = predict(sarima.co2.baru.111.011,12) # peramalan untuk 12 periode
# batas atas
# forecast.co2$pred berisi nilai prediksi (ramalan)
# forecast.co2$se berisi galat standar (standard error)
U <- forecast.co2.baru$pred + 2*forecast.co2.baru$se # batas atas
L <- forecast.co2.baru$pred - 2*forecast.co2.baru$se # batas bawah
min.y <- min(co2.baru,L)# batas minimum untuk sumbu y
max.y <- max(co2.baru,U)# batas maksimum untuk sumbu y
ts.plot(co2.baru,forecast.co2.baru$pred,col=1:2,ylim=c(min.y,max.y))
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")


## Untuk melihat hasil ramalan 
## kita akan potong data co2
cut.co2.baru <- co2.baru[745:757]
cut.co2.baru <- ts(cut.co2.baru,start=c(2020,3),frequency=12)
ts.plot(cut.co2.baru,forecast.co2.baru $pred,col=1:2,ylim=c(min.y,max.y))
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")
abline(v=end(cut.co2.baru),lty="dashed")

