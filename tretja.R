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

