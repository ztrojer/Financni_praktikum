library(actuar)

#KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM

#prva naloga: PORAZDELITEV INDIVIDUALNIH ŠKODNIH ZAHTEVKOV
#1.a: uvozimo txt in narisemo histogram
vzorec <- scan("vzorec3.txt")
histogram <- hist(vzorec, main = "HISTOGRAM", xlab = "Višina odskodnine", ylab = "Frequency", col = "cadetblue1")

#1.b vzamem eksponentno porazdelitev
funkc = mde(vzorec, dexp, start  = list(rate = 1/200), measure = "CvM")
rate = funkc$estimate

#1.c risanje
hist(vzorec, probability = TRUE, main = "HISTOGRAM", xlab = "Višina odskodnine", ylab = "Frequency", col = "cadetblue1")
curve(dexp(x, rate), add = TRUE, col = "red")
legend("topright", legend=c('eksponentna porazelitev'), col = 'red' ,lty=1:1, cex=0.8)

#1.d 
upanje_y <- 1/rate
upanje_s <- upanje_y * 20 * 1/2
var_y = 1/rate^2
var_s = var_y*20*1/2 + 2/rate^2 * 20 * 1/4

#druga naloga
#2.a, 2.b
h = 0.25
n = 100
diskretno <- discretize(pexp(x, rate), 0, h*n, step = h)
plot(diffinv(diskretno))
plot(stepfun(seq(0, (n-1)*h, by = h), diffinv(diskretno)), main = "Eksponentna porazdelitev", xlab = "x", ylab = "Porazdelitvena funkcija")
curve(pexp(x, rate), add = TRUE, col = 'red')
legend("bottomright", legend=c('diskretizacija', 'eksponentna porazelitev'), col=c('black','red'),lty=1:1, cex=0.8)

#2.c Panjerjev algoritem - izračun porazdelitvene funkcije komulativne škode S

porazdelitvena <- aggregateDist(method = 'recursive',
                                model.freq = "binom",
                                model.sev = diskretno,
                                size = 20,
                                prob = 1/2,
                                x.scale = h,
                                maxit=1000000,
                                tol = 0.025)

plot(porazdelitvena)

#2.d izračun upanja in disperzije    
upanje_s_1 <- mean(porazdelitvena)
var_s_1 <- sum(diff(porazdelitvena) * knots(porazdelitvena)^2) - upanje_s_1^2



#tretja naloga
#3.a
Nsim <- rbinom(10000, 20, 1/2)

Ssim<-c()

for (t in Nsim){
  Ssim <- c(Ssim, sum(rexp(t, rate)))
}

#3.b
upanje_s_2 <- mean(Ssim)
var_s_2 <- var(Ssim)

#3.c
plot(ecdf(Ssim),
     col = 'blue',
     add = TRUE)

legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'blue'),
       lty=1:1, cex=0.9)