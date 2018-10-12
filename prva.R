library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
require(readr)
require(dplyr)
require(graphics)

leto13 <- read_csv("podatki/euribor2013.csv", locale=locale(encoding="cp1250"))
leto13 <- leto13[c(1,2,24,44,64,85,107,127,150,172,193,216,237)]
leto13 <- leto13[-c(3,7,8,10,11,13,14),]

leto14 <- read_csv("podatki/euribor2014.csv", locale=locale(encoding="cp1250"))
leto14 <- leto14[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]

leto15 <- read_csv("podatki/euribor2015.csv", locale=locale(encoding="cp1250"))
leto15 <- leto15[c(1,2,23,43,65,85,105,127,150,171,193,215,236)]

tabela <- cbind(leto13, leto14, leto15)
tabela <- tabela[-c(14,27)]
tabela1 <- tabela[,-1]
rownames(tabela1) <- tabela[,1]
tabela <- tabela1
tabela <- t(tabela)


casovna_vrsta1 <- ts(tabela[,6], start = c(2013,1), frequency = 12)
casovna_vrsta2 <- ts(tabela[,8], start = c(2013,1), frequency = 12)

ts.plot(casovna_vrsta1, casovna_vrsta2, col = c("red", "blue"))
