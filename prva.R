library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(tidyr)


#prva naloga
#1.a: uvozimo tabele
leto13 <- read_csv("podatki/euribor2013.csv", locale=locale(encoding="cp1250"))
leto13 <- leto13[c(1,2,24,44,64,85,107,127,150,172,193,216,237)]
leto13 <- leto13[-c(3,7,8,10,11,13,14),]

leto14 <- read_csv("podatki/euribor2014.csv", locale=locale(encoding="cp1250"))
leto14 <- leto14[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]

leto15 <- read_csv("podatki/euribor2015.csv", locale=locale(encoding="cp1250"))
leto15 <- leto15[c(1,2,23,43,65,85,105,127,150,171,193,215,236)]

#1.b: urejanje podatkov

tabela <- cbind(leto13, leto14, leto15)
tabela <- tabela[-c(14,27)]
tabela1 <- tabela[,-1]
rownames(tabela1) <- tabela[,1]
tabela <- tabela1
tabela <- t(tabela)

#1.c: risanje grafa

casovna_vrsta1 <- ts(tabela[,6], start = c(2013,1), frequency = 12)
casovna_vrsta2 <- ts(tabela[,8], start = c(2013,1), frequency = 12)
ts.plot(casovna_vrsta2,casovna_vrsta1, col = c("green", "blue"), main = "Euribor", xlab = "Time", ylab = "%")
legend(x = 2015, y = 0.5, legend = c("12 m", "6 m"),col = c("green", "blue"), lty = 1, bty = "n")

#druga naloga"
#2.a izbrani datumi: "2.12.2013", "1.8.2014", "2.11.2015".

#2.b: urejanje podatkov
tabela2 <- tabela1[c(12,20, 35)]
cas = c(0.25,0.5,1,2,3,6,9,12)
tabela2 <- data.frame(tabela2)
tabela2 <- cbind(tabela2,cas)

graf2 <- plot(y = tabela2[,1],
              x=cas,
              ylim=c(min(-0.2),max(0.6)),
              xlab="Dospetje [mesec]", 
              ylab="%", 
              col="red", 
              main="Casovna struktura Euribor")
lines(tabela2[,c(2)], x=cas,col="deepskyblue", type="o",pch = 10, text(10.5,0.39,"1.8.2014", col="deepskyblue"))
lines(tabela2[,c(1)], x=cas,col="lightsalmon2", type="o",pch = 10, text(10.5,0.53,"1.4.2011", col="lightsalmon2"))
lines(tabela2[,c(3)], x=cas,col="gold", type="o",pch = 10, text(10.5,0,"2.11.2015", col="gold"))

#Opis oblike prikazanih krivulj: oblike krivulj so pričakovane, z večjim dospetjem se obrestna mera poveča.

#tretja naloga
#3.a: racunanje L(0,T,U)
term_obr_m <- c(0)
tabela3 <- tabela[,c(6,8)]
tabela3 <- cbind(tabela3,term_obr_m)

i=1
while (i<36)
{tabela3[i,3] <- 1/(12-6)*((1+12*(as.numeric(as.character(tabela3[i,2]))))/(1 + 6*(as.numeric(as.character(tabela3[i,1]))))-1)
i = i+1
}

#3.b: priprava tabele
Euribor6m <- tabela[,6]
tabela3 <- cbind(tabela3,Euribor6m)
primerjava <- tabela3[,c(0,4,3)]
napoved <- c(c(NA,NA,NA,NA,NA,NA),primerjava[-c(31:36),2])
primerjava[,2] <- napoved

#naredimo tabelo za zadnje grafe

primerjava_graf <- primerjava[c(7:36),]
primerjava_graf <- as.data.frame(primerjava_graf)
primerjava_graf[,1] <- as.numeric(as.character(primerjava_graf[,1]))
primerjava_graf[,2] <- as.numeric(as.character(primerjava_graf[,2]))

l2013 <- primerjava_graf[c(1:6),]
l2014 <- primerjava_graf[c(7:18),]
l2015 <- primerjava_graf[c(19:30),]

#3.c: linearna regresija
graf3 <- plot(primerjava_graf, 
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(0,0.5),
              xlim=c(0,0.5), 
              main="6m Euribor 2013-2015")
points(x=l2013[,2], y = l2013[,1], type = "p", col="red",pch = 16)
points(x=l2014[,2], y = l2014[,1], type = "p", col="blue",pch = 16)
points(x=l2015[,2], y = l2015[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1, lty=2) 
abline(lm(primerjava_graf[,1]~primerjava_graf[,2]),lwd = 2, col = "black")
legend("topleft",bty = "n", c("2013","2014","2015"), pch=16, col =c("red","blue","green"))

#3.d: leto 2013

graf4 <- plot(l2013,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano", 
              ylim=c(0.2,0.4),
              xlim=c(0.2,0.4),
              main="6m Euribor 2013")
points(x=l2013[,2], y = l2013[,1], type = "p", col="red",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(l2013[,1]~l2013[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2011"), pch = 16, col =c("red"))

#3.d: leto 2014
graf5 <- plot(l2014,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(0,0.5), 
              xlim=c(0,0.5),
              main="6m Euribor 2014")
points(x=l2014[,2], y = l2014[,1], type = "p", col="blue",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(l2014[,1]~l2014[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2014"), pch = 16, col =c("blue"))

#3.d: leto 2015
graf6 <- plot(l2015,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(0,0.3),
              xlim=c(0,0.3),
              main="6m Euribor 2015")
points(x=l2015[,2], y = l2015[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(l2015[,1]~l2015[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2015"), pch = 16, col =c("green"))


#Da bi hipoteza pričakovanj trga veljala,bi morali biti napovedana in opazovana obrestna mera
#enaki; torej bi točke ležale na simetrali lihih kvadrantov
#oziroma vsaj v okolici.
#Moji podatki prikazujejo, da se to ne zgodi.