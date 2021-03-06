setwd("C:/Users/Zbyszek/Desktop/WAS/TELEMETRIA")
library ( googlesheets)
library( ggplot2)
library( reshape2)
klucz<-extract_key_from_url("https://docs.google.com/spreadsheets/d/1CwAWrns8CAgizOu4iAHIqb4ckCcDfY_8xPKYr7CAUs4/edit?usp=sharing")
skoroszyt<-gs_key(klucz, visibility = "private")
#############################
# Dane o grupie w wieku 4+
#############################
dane_4plus<-gs_read(skoroszyt, "4_plus")
dane_4plus<-as.data.frame(dane_4plus)
ndane_4plus <- melt(dane_4plus,id=c("miesiac") ) # w uzyciu paciek reshape2
ndane_4plus$variable<-as.character(ndane_4plus$variable)
colnames(ndane_4plus)<-c("miesiac", "program","ogladalnosc")
##############
## udzial w widowni
##############
png('TELEMETRIA_wykresy/4_plus_udzial.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_4plus[ grep("procent",ndane_4plus$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Odsetek widzów w grupie 4+")
dev.off()
##############
## liczebnosc widowni
##############
png('TELEMETRIA_wykresy/4_plus_liczba.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_4plus[ !(1:dim(ndane_4plus)[1]) %in% grep("procent", ndane_4plus$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Liczba widzów w grupie 4+")
dev.off()
##############
## ROZNICE - zmiany rok do roku i procentowe rok do roku
##############
rdane_4plus<-as.matrix(dane_4plus[, !(1:dim(dane_4plus)[2]) %in% grep("procent|miesiac", colnames(dane_4plus) )])
rdane_4plus<- data.frame(  diff( rdane_4plus ,lag = 12) / rdane_4plus[1:(dim( rdane_4plus )[1]-12) ,  ]      )
rdane_4plus$miesiac<-dane_4plus$miesiac[ (dim(dane_4plus)[1]-dim(rdane_4plus)[1]+1):dim(dane_4plus)[1]]
nrdane_4plus <- melt(rdane_4plus,id=c("miesiac") ) # zmiana postaci danych
nrdane_4plus$variable<-as.character(nrdane_4plus$variable)
colnames(nrdane_4plus)<-c("miesiac", "program","ogladalnosc")
##############
## wykres roznic
##############
png('TELEMETRIA_wykresy/4_plus_roznica.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(nrdane_4plus, aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  geom_hline(aes(yintercept=0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Procentowe różnice rok do roku w grupie widzów 4+")
dev.off()
#############################
# Dane o grupie 16-49
#############################
dane_16_49<-gs_read(skoroszyt, "16-49")
dane_16_49<-as.data.frame(dane_16_49)
ndane_16_49 <- melt(dane_16_49,id=c("miesiac") )
ndane_16_49$variable<-as.character(ndane_16_49$variable)
colnames(ndane_16_49)<-c("miesiac", "program","ogladalnosc")
##############
## udzial w widowni
##############
png('TELEMETRIA_wykresy/16_49_udzial.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_16_49[ grep("procent",ndane_16_49$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program,size=1))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Odsetek widzów w grupie 16-49")
dev.off()
##############
## liczebnosc widowni
##############
png('TELEMETRIA_wykresy/16_49_liczba.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_16_49[ !(1:dim(ndane_16_49)[1]) %in% grep("procent", ndane_16_49$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Liczba widzów w grupie 16-49")
dev.off()
##############
## ROZNICE - zmiany rok do roku i procentowe rok do roku
##############
rdane_16_49<-as.matrix(dane_16_49[, !(1:dim(dane_16_49)[2]) %in% grep("procent|miesiac", colnames(dane_16_49) )])
rdane_16_49<- data.frame(  diff( rdane_16_49 ,lag = 12) / rdane_16_49[1:(dim( rdane_16_49 )[1]-12) ,  ]      )
rdane_16_49$miesiac<-dane_16_49$miesiac[ (dim(dane_16_49)[1]-dim(rdane_16_49)[1]+1):dim(dane_16_49)[1]]
nrdane_16_49 <- melt(rdane_16_49,id=c("miesiac") ) # zmiana postaci danych
nrdane_16_49$variable<-as.character(nrdane_16_49$variable)
colnames(nrdane_16_49)<-c("miesiac", "program","ogladalnosc")
##############
## wykres roznic
##############
png('TELEMETRIA_wykresy/16-49_roznica.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(nrdane_16_49, aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  geom_hline(aes(yintercept=0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Procentowe różnice rok do roku w grupie widzów 16-49")
dev.off()
################ SZEREG CZASOWY WIADOMOSCI TVP
library(timeSeries)
dane_4plus$czas<-gsub("/","\\-",dane_4plus$miesiac)
# na liczebnosciach Wiadomosci
dane_4plus_Wiadomosci_liczba_ts<-ts( data = dane_4plus$Wiadomosci_TVP1, start = c(2013,1), frequency = 12)
cycle( dane_4plus_Wiadomosci_liczba_ts)
aggregate( dane_4plus_Wiadomosci_liczba_ts)
time( dane_4plus_Wiadomosci_liczba_ts)
plot( dane_4plus_Wiadomosci_liczba_ts)
abline( reg=lm( dane_4plus_Wiadomosci_liczba_ts~time( dane_4plus_Wiadomosci_liczba_ts) ))
decompose( dane_4plus_Wiadomosci_liczba_ts)
plot( decompose( dane_4plus_Wiadomosci_liczba_ts))
acf( dane_4plus_Wiadomosci_liczba_ts)
# na czestosciach Wiadomosci
dane_4plus_Wiadomosci_czestosc_ts<-ts( data = dane_4plus$Wiadomosci_TVP1_procent, start = c(2013,1), frequency = 12)
cycle( dane_4plus_Wiadomosci_czestosc_ts)
aggregate( dane_4plus_Wiadomosci_czestosc_ts)
time( dane_4plus_Wiadomosci_czestosc_ts)
plot( dane_4plus_Wiadomosci_czestosc_ts, ylim=c(0, max(dane_4plus_Wiadomosci_czestosc_ts)))
abline( reg=lm( dane_4plus_Wiadomosci_czestosc_ts~time( dane_4plus_Wiadomosci_liczba_ts) ), col="red")
## Dekompozycja
decompose( dane_4plus_Wiadomosci_czestosc_ts) # dekompozycha
plot( decompose( dane_4plus_Wiadomosci_czestosc_ts))
plot( stl(dane_4plus_Wiadomosci_czestosc_ts, s.window="periodic"))
## Oszacowanie
plot(dane_4plus_Wiadomosci_czestosc_ts)
abline(lm(dane_4plus_Wiadomosci_czestosc_ts~time(dane_4plus_Wiadomosci_czestosc_ts)),col="red",lwd=2)
acf( dane_4plus_Wiadomosci_czestosc_ts)
## roznice na procentach
hist(diff( dane_4plus_Wiadomosci_czestosc_ts),col="red", breaks=20, ylim=c(0,40))
lines(density(diff( dane_4plus_Wiadomosci_czestosc_ts)),lwd=2)
### dodawanie oszacowanego rozkladu normalnego
mu<-mean(diff(dane_4plus_Wiadomosci_czestosc_ts))
sigma<-sd(diff(dane_4plus_Wiadomosci_czestosc_ts))
x<-seq(-0.04,0.04,length=100)
y<-dnorm(x,mu,sigma)
lines(x,y,lwd=2,col="blue")
### sprawdzanie przy pomocy funkcji "qqnorm" czy rzeczywiscie obserujemy rozklad normalny roznic
qqnorm(diff(dane_4plus_Wiadomosci_czestosc_ts)) # wykres
### sprawdzanie normalnosci przy pomocy testu kolmogorowa
x<-diff(dane_4plus_Wiadomosci_czestosc_ts)
ks.test(x,"pnorm",mean(x),sd(x))
### sprawdzanie normalnosci przy pomocy Shapiro?Test - lepsze rozwiazanie
shapiro.test(x) ### rozklad nie jest zbyt normalny ale troche przypomina wiec jest ok
# Linear Filtering of Time Series
plot(dane_4plus_Wiadomosci_czestosc_ts,type="l")
tui.1 <- filter(dane_4plus_Wiadomosci_czestosc_ts,filter=rep(1/5,5))
tui.2 <- filter(dane_4plus_Wiadomosci_czestosc_ts,filter=rep(1/25,25))
# tui.3 <- filter(dane_4plus_Wiadomosci_czestosc_ts,filter=rep(1/81,81))
lines(tui.1,col="red")
lines(tui.2,col="purple")
#lines(tui.3,col="blue")
# Decomposition of Time Series
plot(stl(dane_4plus_Wiadomosci_czestosc_ts, s.window="periodic")) # dekompozycja sezonowa
# Estimation 
plot( dane_4plus_Wiadomosci_czestosc_ts, ylim=c(0, max(dane_4plus_Wiadomosci_czestosc_ts)))
abline( reg=lm( dane_4plus_Wiadomosci_czestosc_ts~time( dane_4plus_Wiadomosci_liczba_ts) ), col="red")
summary( lm( dane_4plus_Wiadomosci_czestosc_ts~time( dane_4plus_Wiadomosci_liczba_ts)))
# Exponential Smoothing and Prediction of Time Series
plot(dane_4plus_Wiadomosci_czestosc_ts, lwd=2)
lines( fitted(HoltWinters(dane_4plus_Wiadomosci_czestosc_ts))[,1] ,col="red", lwd=2)
lines( fitted(HoltWinters(dane_4plus_Wiadomosci_czestosc_ts))[,2] ,col="green3", lwd=2)
# lines( fitted(HoltWinters(dane_4plus_Wiadomosci_czestosc_ts))[,4] ,col="green3", lwd=2)

lines(HoltWinters(dane_4plus_Wiadomosci_czestosc_ts)$fitted,col="red")
## predict
dane.hw<-HoltWinters(dane_4plus_Wiadomosci_czestosc_ts)
predict(dane.hw,n.ahead=12)
plot(dane_4plus_Wiadomosci_czestosc_ts, xlim=c(2013.0, 2017.15))
lines(predict(dane.hw,n.ahead=48),col="red")
#### predykcja
dane_4plus_Wiadomosci_czestosc_ts_HW<-HoltWinters(dane_4plus_Wiadomosci_czestosc_ts)
predict(dane_4plus_Wiadomosci_czestosc_ts_HW,n.ahead=12)

plot(dane_4plus_Wiadomosci_czestosc_ts,xlim=c(2013, 2018), lwd=2)
lines(predict( dane_4plus_Wiadomosci_czestosc_ts_HW, n.ahead=12), col="red", lwd=2)

### UWAGA !!! MODEL BAZRDZO ZLE PRZEWIDUJE PRZYSZLOSC -> TO JEST WINA OSTATNIEGO ZALAMANIA

# ARIMA–Models
## autocorrelations and partial autocorrelations
## Przyklad ARIMA
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
sim.ma<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")

# ARIMA–Models

fit<-arima( dane_4plus_Wiadomosci_czestosc_ts, order=c(1,0,1))
tsdiag(fit)
Box.test(fit$residuals,lag=2)

# na liczebnosciach FAKTY
dane_4plus_Fakty_liczba_ts<-ts( data = dane_4plus$Fakty_TVN, start = c(2013,1), frequency = 12)
cycle( dane_4plus_Fakty_liczba_ts)
aggregate( dane_4plus_Fakty_liczba_ts)
time( dane_4plus_Fakty_liczba_ts)
plot( dane_4plus_Fakty_liczba_ts)
abline( reg=lm( dane_4plus_Fakty_liczba_ts~time( dane_4plus_Fakty_liczba_ts) ))
decompose( dane_4plus_Fakty_liczba_ts)
plot( decompose( dane_4plus_Fakty_liczba_ts))
# porownanie 
ts.plot( dane_4plus_Fakty_liczba_ts, dane_4plus_Wiadomosci_liczba_ts, col=c("blue", "red") )
