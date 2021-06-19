getwd()
setwd("C:/Users/Home PC/Desktop/MICRO")


analizaMCD_CMG <- read.csv(file="MCD_CMG.csv",header = TRUE,sep=";")
View(analizaMCD_CMG)

ANALIZA.df<-data.frame(analizaMCD_CMG) 
str(ANALIZA.df)

attach(ANALIZA.df)

RentabilitateCMG <- c(0)

for(i in 2:length(Pret_Inchidere_CMG)){
  RentabilitateCMG[length(RentabilitateCMG)+1] <- (Pret_Inchidere_CMG[i] / Pret_Inchidere_CMG[i-1] -1)*100
}

RentabilitateMCD <- c(0)

for(i in 2:length(Pret_Inchidere_MCD)){
  RentabilitateMCD[length(RentabilitateMCD)+1] <- (Pret_Inchidere_MCD[i] / Pret_Inchidere_MCD[i-1] -1)*100
}

RentabilitateSP <- c(0)

for(i in 2:length(Pret_SP)){
  RentabilitateSP[length(RentabilitateSP)+1] <- (Pret_SP[i] / Pret_SP[i-1] -1)*100
}

Rentabilitate.df<-data.frame(ANALIZA.df$Date,RentabilitateCMG,RentabilitateMCD,RentabilitateSP)

View(Rentabilitate.df)

Rentabilitate.df$Round_off <-round(Rentabilitate.df[-1],digit=2)












summary(Pret_Inchidere_CMG)
summary(Pret_Inchidere_MCD)
summary(Pret_SP)
summary(Volum_CMG)
summary(Volum_MCD)
summary(Volum_SP)
summary(RentabilitateCMG)
summary(RentabilitateMCD)
summary(RentabilitateSP)


mean(Pret_Inchidere_CMG)
mean(Pret_Inchidere_MCD)
mean(Pret_SP)
mean(Volum_CMG)
mean(Volum_MCD)
mean(Volum_SP)
mean(RentabilitateCMG)
mean(RentabilitateMCD)
mean(RentabilitateSP)


sd(Pret_Inchidere_CMG)
sd(Pret_Inchidere_MCD)
sd(Pret_SP)
sd(Volum_CMG)
sd(Volum_MCD)
sd(Volum_SP)
sd(RentabilitateCMG)
sd(RentabilitateMCD)
sd(RentabilitateSP)

100*sd(Pret_Inchidere_CMG)/mean(Pret_Inchidere_CMG)
100*sd(Pret_Inchidere_MCD)/mean(Pret_Inchidere_MCD)
100*sd(Pret_SP)/mean(Pret_SP)
100*sd(Volum_CMG)/mean(Volum_CMG)
100*sd(Volum_MCD)/mean(Volum_MCD)
100*sd(Volum_SP)/mean(Volum_SP)
sd(RentabilitateCMG)/mean(RentabilitateCMG)
sd(RentabilitateMCD)/mean(RentabilitateMCD)
sd(RentabilitateSP)/mean(RentabilitateSP)

library(moments)

skewness(Pret_Inchidere_CMG)
skewness(Pret_Inchidere_MCD)
skewness(Pret_SP)
skewness(Volum_CMG)
skewness(Volum_MCD)
skewness(Volum_SP)
skewness(RentabilitateCMG)
skewness(RentabilitateMCD)
skewness(RentabilitateSP)


kurtosis(Pret_Inchidere_CMG)
kurtosis(Pret_Inchidere_MCD)
kurtosis(Pret_SP)
kurtosis(Volum_CMG)
kurtosis(Volum_MCD)
kurtosis(Volum_SP)
kurtosis(RentabilitateCMG)
kurtosis(RentabilitateMCD)
kurtosis(RentabilitateSP)


hist(Pret_Inchidere_CMG, main="Histograma pretului actiunii Chipotle")
hist(Pret_Inchidere_MCD, main="Histograma pretului actiunii McDonald's")
hist(Pret_SP,main="Histograma pretului S&P")
hist(Volum_CMG,main="Histograma volumului de actiuni Chipotle")
hist(Volum_MCD, main="Histograma volumului de actiuni McDonald's")
hist(Volum_SP,main="Histograma volumului de actiuni S&P")
hist(RentabilitateCMG,main="Histograma rentabilitatii Chipotle")
hist(RentabilitateMCD,main="Histograma rentabilitatii McDonald's")
hist(RentabilitateSP,main="Histogramarentabilitatii S&P")


boxplot(Pret_Inchidere_CMG,horizontal = TRUE)
boxplot(Pret_Inchidere_MCD,horizontal = TRUE)
boxplot(Pret_SP,horizontal = TRUE)
boxplot(Volum_CMG,horizontal = TRUE)
boxplot(Volum_MCD,horizontal = TRUE)
boxplot(Volum_SP,horizontal = TRUE)
boxplot(RentabilitateCMG,horizontal = TRUE)
boxplot(RentabilitateMCD,horizontal = TRUE)
boxplot(RentabilitateSP,horizontal = TRUE)

matrice_corelatie <- cor(analizaMCD_CMG[-1])
library(corrplot)
corrplot(matrice_corelatie)




ANALIZA.ts<-zoo(ANALIZA.df[,-1],  order.by = ANALIZA.df$Date)
plot(ANALIZA.ts$Pret_Inchidere_CMG, main="Fluctuatia pretului actiunii Chipotle", ylab = "Price",xlab = "Month",col="red")



#str(ANALIZA.ts)
#plot.ts(ANALIZA.ts$Pret_Inchidere_CMG,Y=NULL)
#class(ANALIZA.df)
#class(ANALIZA.ts)

ANALIZA.df$Date <- as.Date(ANALIZA.df$Date )
Rentabilitate.df$ANALIZA.df.Date<-as.Date(Rentabilitate.df$ANALIZA.df.Date)
str(Rentabilitate.df)
#format(ANALIZA.df$Date, "%m/%d/%Y")
#str(ceva.ts)
#str(ANALIZA.df)
#str(ANALIZA.ts)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
?dplyr
library(plotly)
library(htmlwidgets)
install.packages("plotly")



  
Grafic_Evolutie_Pret_Chipotle<-ggplotly( Grafic_Evolutie_Pret_Chipotle)
saveWidget( Grafic_Evolutie_Pret_Chipotle, file=paste0( getwd(), "/HtmlWidget/GraficEvolutiePretChipotle.html"))


Grafic_Evolutie_Pret_McDonalds <- (ggplot(ANALIZA.df,aes(x=Date,y=Pret_Inchidere_MCD))+
                                    geom_area(fill="#ECA3F5", alpha=0.5) +
                                    geom_line(color="#ECA3F5") +
                                    ylab("Price") +
                                    xlab("Date")+
                                    theme_light() )
Grafic_Evolutie_Pret_McDonalds<-ggplotly( Grafic_Evolutie_Pret_McDonalds)
saveWidget( Grafic_Evolutie_Pret_McDonalds, file=paste0( getwd(), "/HtmlWidget/GraficEvolutiePretMcDonald's.html"))


Grafic_Evolutie_Pret_SP <- (ggplot(ANALIZA.df,aes(x=Date,y=Pret_SP))+
                                     geom_area(fill="#ECA3F5", alpha=0.5) +
                                     geom_line(color="#ECA3F5") +
                                     ylab("Price") +
                                     xlab("Date")+
                                     theme_light() )
Grafic_Evolutie_Pret_SP<-ggplotly( Grafic_Evolutie_Pret_SP)
saveWidget( Grafic_Evolutie_Pret_SP, file=paste0( getwd(), "/HtmlWidget/GraficEvolutiePretS&P500.html"))




Grafic_Evolutie_Volum_Actiuni_Chipotle <- (ggplot(ANALIZA.df,aes(x=Date,y=Volum_CMG))+
                              geom_area(fill="#ECA3F5", alpha=0.5) +
                              geom_line(color="#ECA3F5") +
                              ylab("Volum actiuni") +
                              xlab("Date")+
                              theme_light() )
Grafic_Evolutie_Volum_Actiuni_Chipotle<-ggplotly( Grafic_Evolutie_Volum_Actiuni_Chipotle)
saveWidget( Grafic_Evolutie_Volum_Actiuni_Chipotle, file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Volum_Actiuni_Chipotle.html"))




Grafic_Evolutie_Volum_Actiuni_McDonalds <- (ggplot(ANALIZA.df,aes(x=Date,y=Volum_MCD))+
                                             geom_area(fill="#ECA3F5", alpha=0.5) +
                                             geom_line(color="#ECA3F5") +
                                             ylab("Volum actiuni") +
                                             xlab("Date")+
                                             theme_light() )
Grafic_Evolutie_Volum_Actiuni_McDonalds <-ggplotly(Grafic_Evolutie_Volum_Actiuni_McDonalds )
saveWidget( Grafic_Evolutie_Volum_Actiuni_McDonalds , file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Volum_Actiuni_McDonalds.html"))




Grafic_Evolutie_Volum_Actiuni_SP <- (ggplot(ANALIZA.df,aes(x=Date,y=Volum_SP))+
                                              geom_area(fill="#ECA3F5", alpha=0.5) +
                                              geom_line(color="#ECA3F5") +
                                              ylab("Volum actiuni") +
                                              xlab("Date")+
                                              theme_light() )
Grafic_Evolutie_Volum_Actiuni_SP <-ggplotly(Grafic_Evolutie_Volum_Actiuni_SP )
saveWidget( Grafic_Evolutie_Volum_Actiuni_SP , file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Volum_Actiuni_SP.html"))






Grafic_Evolutie_Rentabilitate_Actiune_Chipotle <- (ggplot(ANALIZA.df,aes(x=Rentabilitate.df$ANALIZA.df.Date,y=Rentabilitate.df$Round_off$RentabilitateCMG))+
                                       geom_area(fill="#ECA3F5", alpha=0.5) +
                                       geom_line(color="#ECA3F5") +
                                       ylab("Rentabilitate actiune") +
                                       xlab("Data")+
                                       theme_light() )
Grafic_Evolutie_Rentabilitate_Actiune_Chipotle <-ggplotly(Grafic_Evolutie_Rentabilitate_Actiune_Chipotle )
saveWidget( Grafic_Evolutie_Rentabilitate_Actiune_Chipotle , file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Rentabilitate_Actiune_Chipotle.html"))



Grafic_Evolutie_Rentabilitate_Actiune_McDonalds <- (ggplot(ANALIZA.df,aes(x=Rentabilitate.df$ANALIZA.df.Date,y=Rentabilitate.df$Round_off$RentabilitateMCD))+
                                                     geom_area(fill="#ECA3F5", alpha=0.5) +
                                                     geom_line(color="#ECA3F5") +
                                                     ylab("Rentabilitate actiune") +
                                                     xlab("Data")+
                                                     theme_light() )
Grafic_Evolutie_Rentabilitate_Actiune_McDonalds <-ggplotly(Grafic_Evolutie_Rentabilitate_Actiune_McDonalds)
saveWidget( Grafic_Evolutie_Rentabilitate_Actiune_McDonalds , file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Rentabilitate_Actiune_McDonalds.html"))




Grafic_Evolutie_Rentabilitate_Actiune_SP <- (ggplot(ANALIZA.df,aes(x=Rentabilitate.df$ANALIZA.df.Date,y=Rentabilitate.df$Round_off$RentabilitateSP))+
                                                      geom_area(fill="#ECA3F5", alpha=0.5) +
                                                      geom_line(color="#ECA3F5") +
                                                      ylab("Rentabilitate actiune") +
                                                      xlab("Data")+
                                                      theme_light() )
Grafic_Evolutie_Rentabilitate_Actiune_SP <-ggplotly(Grafic_Evolutie_Rentabilitate_Actiune_SP)
saveWidget( Grafic_Evolutie_Rentabilitate_Actiune_SP , file=paste0( getwd(), "/HtmlWidget/Grafic_Evolutie_Rentabilitate_Actiune_SP"))



  


############################################HISTOGRAME##################################################

Histograma_Actiune_CMG <- (ggplot(ANALIZA.df,aes(x=Pret_Inchidere_CMG))+
                             geom_histogram( binwidth=100, fill="#d92b27", color="#ffc82c", alpha=0.9) +
                             ggtitle("Histograma pretului Chipotle Mexican Grill (FIG 1)") +
                             xlab("Pret actiune Chipotle")+
                             ylab("Frecventa")+
                             theme_light() +
                             theme(
                             plot.title = element_text(size=13)
                             ))
Histograma_Actiune_CMG <-ggplotly(Histograma_Actiune_CMG)
saveWidget( Histograma_Actiune_CMG , file=paste0( getwd(), "/HtmlWidget/FIG1_Histograma_Actiune_CMG.html"))






Histograma_Actiune_MCD <- (ggplot(ANALIZA.df,aes(x=Pret_Inchidere_MCD))+
                             geom_histogram( binwidth=10, fill="#d92b27", color="#ffc82c", alpha=0.9) +
                             ggtitle("Histograma pretului McDonald's (FIG 2)") +
                             xlab("Pret actiune McDonal'd")+
                             ylab("Frecventa")+
                             theme_light() +
                             theme(
                               plot.title = element_text(size=13)
                             ))
Histograma_Actiune_MCD <-ggplotly(Histograma_Actiune_MCD)
saveWidget( Histograma_Actiune_MCD , file=paste0( getwd(), "/HtmlWidget/FIG2_Histograma_Actiune_MCD.html"))




Histograma_Actiune_SP <- (ggplot(ANALIZA.df,aes(x=Pret_SP))+
                             geom_histogram( binwidth=100, fill="#d92b27", color="#ffc82c", alpha=0.9) +
                             ggtitle("Histograma pretului SP (FIG 3)") +
                             xlab("Pret actiune SP")+
                             ylab("Frecventa")+
                             theme_light() +
                             theme(
                               plot.title = element_text(size=13)
                             ))
Histograma_Actiune_SP <-ggplotly(Histograma_Actiune_SP)
saveWidget( Histograma_Actiune_SP , file=paste0( getwd(), "/HtmlWidget/FIG3_Histograma_Actiune_SP.html"))


Histograma_Volum_CMG <- (ggplot(ANALIZA.df,aes(x=Volum_CMG))+
                             geom_histogram( binwidth=10, fill="#d92b27", color="#ffc82c", alpha=0.9) +
                             ggtitle("Histograma volumului de actiuni Chipotle (FIG 4)") +
                             xlab("Volum actiuni Chipotle")+
                             ylab("Frecventa")+
                             theme_light() +
                             theme(
                               plot.title = element_text(size=13)
                             ))
Histograma_Volum_CMG <-ggplotly(Histograma_Volum_CMG)
saveWidget( Histograma_Volum_CMG , file=paste0( getwd(), "/HtmlWidget/FIG4_Histograma_Volum_CMG.html"))

































par(bty="o")
boxplot(Pret_Inchidere_CMG,col = "#d92b27", horizontal=TRUE)


OutliersCMG<-boxplot(Pret_Inchidere_CMG)$out
which(Pret_Inchidere_CMG %in% OutliersCMG)
Date[which(Pret_Inchidere_CMG %in% OutliersCMG)]













par(mfrow=c(2,2),mar=c(2,2,2,2))
plot.ts(Pret_Inchidere_CMG,type="l",col="Red",main="CMG")
plot(Pret_Inchidere_MCD,type="l",col="magenta", main="MCD")
plot(Pret_SP,type="l",col="purple",main = "S&P")
dev.off()





ANALIZA.df$Pret_Inchidere_CMG[which.max(ANALIZA.df$Pret_Inchidere_CMG)]
ANALIZA.df$Date[which.max(ANALIZA.df$Pret_Inchidere_CMG)] #"2021-02-09"

ANALIZA.df$Pret_Inchidere_CMG[which.min(ANALIZA.df$Pret_Inchidere_CMG)]
ANALIZA.df$Date[which.min(ANALIZA.df$Pret_Inchidere_CMG)]  ##"2020-03-18"



ANALIZA.df$Pret_Inchidere_MCD[which.max(ANALIZA.df$Pret_Inchidere_MCD)]
ANALIZA.df$Date[which.max(ANALIZA.df$Pret_Inchidere_MCD)] #"2020-10-15"

ANALIZA.df$Pret_Inchidere_MCD[which.min(ANALIZA.df$Pret_Inchidere_MCD)]
ANALIZA.df$Date[which.min(ANALIZA.df$Pret_Inchidere_MCD)]  ##"2020-03-23"



ANALIZA.df$Pret_SP[which.max(ANALIZA.df$Pret_SP)]
ANALIZA.df$Date[which.max(ANALIZA.df$Pret_SP)]    #"2021-02-12"   

ANALIZA.df$Pret_SP[which.min(ANALIZA.df$Pret_SP)]
ANALIZA.df$Date[which.min(ANALIZA.df$Pret_SP)]   ##"2020-03-23"









par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(Volum_CMG,type="l",col="Red",main="CMG")
plot(Volum_MCD,type="l",col="magenta", main="MCD")
plot(Volum_SP,type="l",col="purple",main = "S&P")
dev.off()

par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(RentabilitateCMG,type="l",col="Red",main="CMG")
plot(RentabilitateMCD,type="l",col="magenta", main="MCD")
plot(RentabilitateSP,type="l",col="purple",main = "S&P")
dev.off()

nrValoriNegative=0 
nrValoriPozitive=0

for(i in 1:length(RentabilitateCMG)){
  if(RentabilitateCMG[i]<0) 
    nrValoriNegative=nrValoriNegative+1  #118
  else
    nrValoriPozitive=nrValoriPozitive+1  #133
}


for(i in 1:length(RentabilitateMCD)){
  if(RentabilitateMCD[i]<0) 
    nrValoriNegative=nrValoriNegative+1   #118
  else
    nrValoriPozitive=nrValoriPozitive+1   #133
}

nrValoriNegative
nrValoriPozitive


nrValoriSubMedie=0 
nrValoriPesteMedie=0

mediaValorilorSubMedie=0
mediaValorilorPesteMedie=0

sumaVloriSubMedie=0
sumaVloriPesteMedie=0


for(i in 1:length(RentabilitateCMG)){
  if(RentabilitateCMG[i]<mean(RentabilitateCMG)) {
    nrValoriSubMedie=nrValoriSubMedie+1  #132                       #mediaSUB=-1.69
    sumaVloriSubMedie=sumaVloriSubMedie+RentabilitateCMG[i]
  }                                                                 #MEDIA=0.29
  else{
    nrValoriPesteMedie=nrValoriPesteMedie+1  #119                   #mediaPESTE=2.50
    sumaVloriPesteMedie=sumaVloriPesteMedie+RentabilitateCMG[i]
  }                                                              
}


for(i in 1:length(RentabilitateMCD)){
  if(RentabilitateMCD[i]<mean(RentabilitateMCD)) {
    nrValoriSubMedie=nrValoriSubMedie+1  #132                  #mediaSUB= -0.80
    sumaVloriSubMedie=sumaVloriSubMedie+RentabilitateCMG[i]
  }                                                            #MEDIA=0.04
  else{
    nrValoriPesteMedie=nrValoriPesteMedie+1  #119              #mediaPESTE= 1.43
    sumaVloriPesteMedie=sumaVloriPesteMedie+RentabilitateCMG[i]
  }
}

mediaValorilorSubMedie <- sumaVloriSubMedie/nrValoriSubMedie
mediaValorilorPesteMedie <- sumaVloriPesteMedie/nrValoriPesteMedie

mediaValorilorSubMedie
mediaValorilorPesteMedie

for(i in 1:length(RentabilitateMCD)){
  if(RentabilitateMCD[i]<mean(RentabilitateMCD)) 
    nrValoriSubMedie=nrValoriSubMedie+1  #128
  else
    nrValoriPesteMedie=nrValoriPesteMedie+1  #123
}

nrValoriSubMedie
nrValoriPesteMedie









