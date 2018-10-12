library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)


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
tabela3 <- tabela1[c(12,20, 35)]
cas = c(0.25,0.5,1,2,3,6,9,12)
tabela3 <- data.frame(tabela3)
tabela3 <- cbind(tabela3,cas)

graf2 <- plot(y = tabela3[,1],
                    x=cas,
                    ylim=c(min(-0.2),max(0.6)),
                    xlab="Dospetje [mesec]", 
                    ylab="%", 
                    col="red", 
                    main="Casovna struktura Euribor")
lines(tabela3[,c(2)], x=cas,col="deepskyblue", type="o",pch = 10, text(10.5,0.39,"1.8.2014", col="deepskyblue"))
lines(tabela3[,c(1)], x=cas,col="lightsalmon2", type="o",pch = 10, text(10.5,0.53,"1.4.2011", col="lightsalmon2"))
lines(tabela3[,c(3)], x=cas,col="gold", type="o",pch = 10, text(10.5,0,"2.11.2015", col="gold"))


#Opis oblike prikazanih krivulj: oblike krivulj so pričakovane, z večjim dospetjem se obrestna mera poveča.
