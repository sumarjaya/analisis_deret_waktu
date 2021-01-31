## Mempersiapkan direktori kerja
setwd("D:\\my-campus\\catatan-kuliah\\2013\\adw\\workdir\\01")

## Mengakses pustaka datasets
## Contoh 1.2.1
## Data AirPassengers
library(datasets)
help(datasets)
plot(AirPassengers,xlab="Tahun",ylab="Jumlah Penumpang")
start(AirPassengers)
end(AirPassengers)


## load library TSA
## Contoh 1.2.1 dengan penambahan karakter plot
## Data AirPassengers
library(TSA)
plot(AirPassengers,type="l",xlab="Tahun",ylab="Jumlah Penumpang")
points(y=AirPassengers,x=time(AirPassengers),pch=as.vector(season(AirPassengers)))

# Contoh 1.2.2
# Data CO2
# win.graph(width=4,height=2)
plot(co2,xlab="Tahun",ylab="Konsentrasi CO2 (ppm)")


# Contoh 1.2.3
# Data saham Facebook
FB <- read.csv("FB.csv",header=T) # membaca data saham Facebook
FB.Close <- FB$Close # hanya mengakses data penutupan (Closing)
par(mfrow=c(2,1)) # membuat tampilan menjadi 2 baris, 1 kolom
plot.ts(FB.Close,main="Harga penutupan saham Facebook") # memplot data
LR.FB.Close <- diff(log(FB.Close)) # menghitung log return
plot.ts(LR.FB.Close,main="Log return penutupan saham Facebook") # memplot log
 # return

# Contoh 1.2.4
# Data EEG
EEG <- scan("EEG.txt")
plot.ts(EEG)

# Contoh 1.2.5
wolfer <- scan("wolfer.txt",skip=1)
plot.ts(wolfer)

# Contoh 1.2.6
# Data Google
Google <- read.csv("GOOGL.csv",header=T) # membaca data saham Google
Google.Close <- Google$Close # mengakses harga penutupan
LR.Google.Close <- diff(log(Google.Close)) # menghitung log return
par(mfrow=c(2,1)) # membuat tampilan 2 baris, 1 kolom
plot.ts(Google.Close,main="Harga penutupan saham Google") # plot penutupan
plot.ts(LR.Google.Close,main="Log return penutupan saham Google") # plot
# log return

# Contoh 1.2.7
# Data SOI
SOI <- scan("SOI.txt")
plot.ts(SOI)

# Contoh 1.2.8
# Data PopUSA
PopUSA <- read.table("PopUSA.txt",skip=3)
PopUSA <- PopUSA[,3]
plot.ts(PopUSA,xlab="Tahun",ylab="Jumlah Populasi")

# Contoh 1.2.9
# Data ldeaths
plot(ldeaths)

# Contoh 1.2.10
# Data gol pertandingan
goals <- read.table("goals.txt")
plot.ts(goals[,1])

# Contoh 1.2.11
# Data lynx
plot(lynx,xlab="Tahun")

# ACF dan PACF data lynx
par(mfrow=c(2,1))
acf(lynx,type="covariance",main="Autokovarians Lynx")
acf(lynx,main="Autokorelasi Lynx")

# Periodogram lynx
lynx.per = spec.pgram(lynx,taper=0,log="no")
abline(v=1/4,lty="dotted")

# Contoh 1.5.1
# Data fMRI
library(astsa)
par(mfrow=c(2,1))
ts.plot(fmri1[,2:5],col=1:4,ylab="BOLD",main="Cortex")
ts.plot(fmri1[,6:9],col=1:4,ylab="BOLD",main="Thalamus & Cerebellum")

# Contoh 1.6.1
# Simulasi White Noise
set.seed(1234)
WN = rnorm(1000,0,1)
plot.ts(WN,main="White Noise")

# Contoh 1.6.2
# Rata-rata bergerak
V = filter(WN, sides=2, filter=rep(1/3,3))
plot.ts(V,ylim=c(-3,3), main="Rata-rata Bergerak")

# Contoh 1.6.2
# Langkah acak dengan hanyutan (random walk with drift)
set.seed(1235)
W = rnorm(500)
X = cumsum(W)
WD = W + 0.2
XD = cumsum(WD)
plot.ts(XD,main="Langkah acak",ylab="")
lines(X,col = 4)
abline(h = 0,col = 4,lty = 2)
abline(a = 0, b = 0.2, lty = 2)

# Differencing
par(mfrow=c(2,1))
plot(LakeHuron,main="Lake Huron")
plot(diff(LakeHuron),main="Lake Huron yang Differencing")

# Transformasi logaritma 
log.AirPass = log(AirPassengers)
par(mfrow=c(2,1))
plot(AirPassengers,main="Air Passengers")
plot(log.AirPass,main="Log Air Passengers")

