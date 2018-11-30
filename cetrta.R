require(readr)

#prva
#a
srebro <- read_csv("srebro.csv", locale=locale(encoding="cp1250"))
srebro <- srebro[c(126:1),c(5)]
srebro$Close <- as.numeric(gsub("\\$", "", srebro$Close))

#b
casovna <- ts(srebro)
graf_srebro <- ts.plot(casovna, xlab = "Leto", ylab = "Vrednost srebra v evrih", main = "Graf vrednosti srebra")

#druga
#a
G <- function(vrsta, k) {
  dolzina <- length(vrsta)
  glajenje_vrednosti <- c()
  for (i in 1:(dolzina-k)) {
    glajenje_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zglajena_vrsta <- ts(glajenje_vrednosti)
  return(zglajena_vrsta)
}

#b
zglajena_vrsta_5 <- G(casovna, 5)

dolzina <- length(casovna)
napoved5 <- sum(casovna[(dolzina-5+1):dolzina])/5

#c
graf2 <- ts.plot(casovna, zglajena_vrsta_5, xlab = "Leto", ylab = "Vrednost srebra", lwd = 2:1, col = 1:10)

#d
SKN <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
    return(napaka)
}

Napaka5 <- SKN(casovna, zglajena_vrsta_5, 5)

#e
zglajena_vrsta_15 <- G(casovna, 15)
napoved15 <- sum(casovna[(dolzina-15+1):dolzina])/15
Napaka15 <- SKN(casovna, zglajena_vrsta_15, 15)

zglajena_vrsta_30 <- G(casovna, 30)
napoved30 <- sum(casovna[(dolzina-30+1):dolzina])/30
Napaka30 <- SKN(casovna, zglajena_vrsta_30, 30)


par(mfrow = (c(2,2)))
graf2 <- ts.plot(casovna, zglajena_vrsta_5, xlab = "Leto", ylab = "Vrednost srebra",main = "Drseèe povpreèje reda 5", lwd = 2:1, col = 1:10)
graf3 <- ts.plot(casovna, zglajena_vrsta_15,xlab = "Leto", ylab = "Vrednost srebra",main = "Drseèe povpreèje reda 15", lwd = 2:1, col = 1:10)
graf4 <- ts.plot(casovna, zglajena_vrsta_30,xlab = "Leto", ylab = "Vrednost srebra",main = "Drseèe povpreèje reda 30", lwd = 2:1, col = 1:10)
par(mfrow = (c(1,1)))


#3
#a
EG <- function(vrsta, alpha) {
  glajena_vrsta <- c(vrsta[1])
  dolzina <- length(vrsta)
  for (i in 2:dolzina) {
    glajena_vrsta[i] <- alpha*vrsta[i] + (1-alpha)*glajena_vrsta[i-1]
  }
  return(ts(glajena_vrsta))
}
 #b
alfa = 0.2
zglajena_vrsta_alfa <- EG(casovna, alfa)

graf6 <- ts.plot(casovna, zglajena_vrsta_alfa,xlab = "Leto", ylab = "Vrednost srebra",main = "Eksponentno glajenje", lwd = 2:1, col = 1:10)

#c
SKN_o <- function(vrsta, alfa) {
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alfa)
  for (i in 1:(dolzina-1)) {
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina -1))
}

opt_alfa <- optimize(SKN_o, c(0,1), vrsta = casovna)

#d
zglajen_alfa_o <- EG(casovna, opt_alfa$minimum)

graf7 <- ts.plot(casovna, zglajen_alfa_o, ylab = "EUR", main = "Eksponentsno glajenje, minimalen MSE",lwd = 3:1, col = 10:1)
