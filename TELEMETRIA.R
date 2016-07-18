library ( googlesheets)
library( ggplot2)
klucz<-extract_key_from_url("https://docs.google.com/spreadsheets/d/1CwAWrns8CAgizOu4iAHIqb4ckCcDfY_8xPKYr7CAUs4/edit?usp=sharing")
skoroszyt<-gs_key(klucz, visibility = "private")
#############################
# Dane o grupie w wieku 4+
#############################
dane_4plus<-gs_read(skoroszyt, "4_plus")
dane_4plus<-as.data.frame(dane_4plus)
ndane_4plus <- reshape2::melt(dane_4plus,id=c("miesiac") ) # w uzyciu paciek reshape2
ndane_4plus$variable<-as.character(ndane_4plus$variable)
colnames(ndane_4plus)<-c("miesiac", "program","ogladalnosc")
##############
## udzial w widowni
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/4_plus_udzial.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_4plus[ grep("procent",ndane_4plus$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Odsetek widzów w grupie 4+")
dev.off()
##############
## liczebnosc widowni
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/4_plus_liczba.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
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
nrdane_4plus <- reshape2::melt(rdane_4plus,id=c("miesiac") ) # zmiana postaci danych
nrdane_4plus$variable<-as.character(nrdane_4plus$variable)
colnames(nrdane_4plus)<-c("miesiac", "program","ogladalnosc")
##############
## wykres roznic
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/4_plus_roznica.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
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
ndane_16_49 <- reshape2::melt(dane_16_49,id=c("miesiac") )
ndane_16_49$variable<-as.character(ndane_16_49$variable)
colnames(ndane_16_49)<-c("miesiac", "program","ogladalnosc")
##############
## udzial w widowni
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/16_49_udzial.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(ndane_16_49[ grep("procent",ndane_16_49$program),], aes(x=miesiac , y=ogladalnosc, group=program, color=program,size=1))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Odsetek widzów w grupie 16-49")
dev.off()
##############
## liczebnosc widowni
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/16_49_liczba.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
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
nrdane_16_49 <- reshape2::melt(rdane_16_49,id=c("miesiac") ) # zmiana postaci danych
nrdane_16_49$variable<-as.character(nrdane_16_49$variable)
colnames(nrdane_16_49)<-c("miesiac", "program","ogladalnosc")
##############
## wykres roznic
##############
png('C:/Users/Zbyszek/Desktop/TELEMETRIA_wykresy/16-49_roznica.png',  res = 200, width = 20, height = 10, units = "cm") #bg = "white", 
ggplot(nrdane_16_49, aes(x=miesiac , y=ogladalnosc, group=program, color=program))+
  geom_line()+
  geom_hline(aes(yintercept=0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Procentowe różnice rok do roku w grupie widzów 16-49")
dev.off()
################ 
library(ts)
