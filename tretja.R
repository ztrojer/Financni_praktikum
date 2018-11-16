library(combinat)
library(Rlab)

#prva
S0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03
W = c(1,2,3,4,5,6)

pot1 = c(50, 52.50, 49.88, 47.38, 45.01, 47.26)
pot2 = c(50, 52.50, 52.12, 57.88, 60.78, 63.81)
pot3 = c(50, 47.50, 49.88, 47.38, 45.01, 42.76)
pot4 = c(50, 47.50, 45.12, 47.38, 45.01, 47.26)
pot5 = c(50, 52.50, 49.88, 52.37, 54.99, 52.24)

#b
izplacilo <- function(vrsta, W, type) {
  K = 0
  l = 0
  for (i in 1:length(W)) {
    K = K + W[i] * vrsta[i]
    l = l + W[i]
  } 
  if (type == "call") {
    izplacilo = max(vrsta[length(vrsta)] - K/l, 0)
    
  } else {
    izplacilo = max(K/l - vrsta[length(vrsta)], 0)
  }
  izplacilo
}
#a
izplaciloX1 = izplacilo(pot1, W, "call")
izplaciloX2 = izplacilo(pot2, W, "call")
izplaciloX3 = izplacilo(pot3, W, "call")
izplaciloX4 = izplacilo(pot4, W, "call")
izplaciloX5 = izplacilo(pot5, W, "call")

izplaciloy1 = izplacilo(pot1, W, "put")
izplaciloy2 = izplacilo(pot2, W, "put")
izplaciloy3 = izplacilo(pot3, W, "put")
izplaciloy4 = izplacilo(pot4, W, "put")
izplaciloy5 = izplacilo(pot5, W, "put")

#druga
binomski <- function(S0, u, d, R, T, W, type) {
  q = (1+R-d)/(u-d)
  hc = hcube(rep(2,T)) - 1
  stevilo = rowSums(hc)
  Q = q ** stevilo * (1-q) ** (T-stevilo)
  poti = u ** hc * (d ** (1 - hc))
  drevo <- cbind(rep(S0, 2**T), poti)
  kom_prod = t(apply(drevo, 1, cumprod))
  izplacila <- apply(kom_prod, 1, function(x) izplacilo(x,W,type))
  E <- sum(izplacila * Q)
  return (E/(1+R)^T)
}

#b
monte <- function(S0, u, d, R, T, W, type, N) {
  q = (1+R-d)/(u-d)
  stanja <- matrix(rbinom(T*N,1,q),N,T) 
  stanja_1 <- d**(1-stanja) * u**(stanja)
  
  k <- rowSums(stanja)
  Q <- q**k *(1-q)**(T-k)
  
  stanja_1 <- t(apply(stanja_1, 1, cumprod))
  vrednosti <- cbind(S0, S0*stanja_1)
  
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x,W,type))
  E = sum(izplacila)/ length(izplacila)
  return (E/(1+R)^T)
}

monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put", 10)
monte(60, 1.05, 0.95, 0.01,15, rep(1,16), "put", 100)
monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put",1000)

#tretja
N1 <- c()
N2 <- c()
N3 <- c()

for (i in c(1:100)){
  N1 <- c(N1,monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put", 10) )
  N2 <- c(N2,monte(60, 1.05, 0.95, 0.01,15, rep(1,16), "put", 100) )
  N3 <- c(N3,monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put",1000) )
}
min <- floor(min(c(N1,N2,N3))) 
max <- ceiling(max(c(N1,N2,N3))) 

cena_binomske <- binomski(60,1.05, 0.95,0.01, 15, rep(1,16), "put")

#histogram N1
pov.N1 <- mean(N1)
odklon.N1 <- sqrt(var(N1))
x1_odklon_desno <- cena_binomske + odklon.N1
x1_odklon_levo <- cena_binomske - odklon.N1

histogram1 <-hist(N1,breaks = 7,
                  main = "Monte Carlo: N=10",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N1, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x1_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x1_odklon_levo, col= "green", length = 0.1 )

legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

#histogram N2
pov.N2 <- mean(N2)
odklon.N2 <- sqrt(var(N2))
x2_odklon_desno <- cena_binomske + odklon.N2
x2_odklon_levo <- cena_binomske - odklon.N2


histogram2 <-hist(N2,breaks = 7,
                  main = "Monte Carlo: N=100",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N2, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x2_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x2_odklon_levo, col= "green", length = 0.1 )


legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

#histogram N3
pov.N3 <- mean(N3)
odklon.N3 <- sqrt(var(N3))
x3_odklon_desno <- cena_binomske + odklon.N3
x3_odklon_levo <- cena_binomske - odklon.N3


histogram3 <-hist(N3,breaks = 7,
                  main = "Monte Carlo: N=1000",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N3, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x3_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x3_odklon_levo, col= "green", length = 0.1 )
legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))


