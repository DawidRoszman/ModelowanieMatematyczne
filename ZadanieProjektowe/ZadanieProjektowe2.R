#http://drageusgames.com/
install.packages("mnormt")
install.packages("MASS")
install.packages("QRM")

library(fitdistrplus)
library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

# DRG

ccc_d <- read.csv("D:/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_d <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
kurs_zamknieca_drg <- ccc_d$Zamkniecie

ccc_s <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/swg_d.csv")
kurs_zamkniecia_swg <- ccc_s$Zamkniecie

data <- as.Date(ccc_d$Data)

plot(data, kurs_zamknieca_drg, xlab = "Data", ylab = "Kurs zamknięcia")

hist(kurs_zamknieca_drg, probability = TRUE, xlab = "Kurs zamknięcia", main = "Histogram kursów zamknięcia")

sd(kurs_zamknieca_drg)

descdist(kurs_zamknieca_drg)


fnorm <- fitdist(kurs_zamknieca_drg, "norm")
fln <- fitdist(kurs_zamknieca_drg, "lnorm")
fg <- fitdist(kurs_zamknieca_drg, "gamma")

fnorm
fln
fg

denscomp(list(fnorm, fln, fg))
qqcomp(list(fnorm, fln, fg))
cdfcomp(list(fnorm, fln, fg))
ppcomp(list(fnorm, fln, fg))

gofstat(list(fnorm, fln, fg))

N <- 1000
n <- length(kurs_zamknieca_drg)

D <- c()

shape <- fg$estimate[[1]]
rate <- fg$estimate[[2]]

for (i in 1:N) { 
  
  Y <- rgamma(n, shape, rate) 
  D[i] <- ks.test(Y,pgamma,shape = shape, rate = rate,exact=TRUE)$statistic
}

dn <- ks.test(kurs_zamknieca_drg,pgamma,shape = shape, rate = rate,exact=TRUE)$statistic
hist(D,prob=T)
points(dn,0,pch=19,col=2)

p_value <- length(D[D>dn])/N; p_value

alpha <- 0.05
p_value <= alpha

# SWG 

library(fitdistrplus)

# 1

kurs_zam <- swg_d$Zamkniecie
par(mfrow=c(1,1))
plot(kurs_zam, type = "l", col = "blue",
     main = paste("Wykres cen zamknięcia dla spółki Seco/Warwick"),
     ylab = "Cena zamknięcia", xlab = "Data")

hist(kurs_zam, prob=T, main = paste("Histogram cen zamknięcia dla spółki Seco/Warwick"), ylab = "Gęstość", xlab = "Cena zamknięcia")

# 2

descdist(kurs_zam)

# 3

fitNorm <- fitdist(kurs_zam, "norm")
fitLogNorm <- fitdist(kurs_zam, "lnorm")
fitExp <- fitdist(kurs_zam, "exp")
fitGamma <- fitdist(kurs_zam, "gamma")

fexp; fln; fnorm; fgamma

# 4

plot.legend <- c("normal", "lognormal", "exp", "gamma")

denscomp(list(fitNorm, fitLogNorm, fitExp, fitGamma), legendtext = plot.legend)

fexp <- fitdist(kurs_zam, "exp")
fln <- fitdist(kurs_zam, "lnorm")
fnorm <- fitdist(kurs_zam, "norm")
fgamma <- fitdist(kurs_zam, "gamma")


plot.legend <- c('exp','log-norm', 'norm', 'gamma')

denscomp(list(fexp,fln,fnorm,fgamma),legendtext =plot.legend)
cdfcomp(list(fexp,fln,fnorm,fgamma),legendtext =plot.legend)
qqcomp(list(fexp,fln,fnorm,fgamma),legendtext =plot.legend)

# 5

gofstat(list(fexp, fln, fnorm, fgamma))

n <- length(kurs_zam); n
N <- 10000

Dln <- c()

for (i in 1:N) {
  
  Yln <- rlnorm(n,fln$estimate[1],fln$estimate[2])
  
  Dln[i] <-  ks.test(Yln,plnorm, fln$estimate[1],fln$estimate[2],exact=TRUE)$statistic
}

dn_ln <-  ks.test(kurs_zam,plnorm,fln$estimate[[1]],fln$estimate[[2]],exact=TRUE)$statistic
dn_ln

par(mfrow=c(1,1))
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)

# ks.test(kurs_zam,plnorm,fln$estimate[[1]],fln$estimate[[2]],exact=TRUE)
p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln


# PART II

# Spółka I
y_drg <- log(kurs_zamknieca_drg)
r_drg <- diff(y_drg)
plot(r_drg)
plot(kurs_zamknieca_drg)
hist(r_drg, probability = T)
fnorm_r_drg <- fitdist(r_drg, "norm")

# MC

mean_drg <- fnorm_r_drg$estimate[1]
sd_drg <- fnorm_r_drg$estimate[2]

N <- 1000
n <- length(r_drg)

D_drg <- c()

for (i in 1:N) { 
  
  Y <- rnorm(n, mean_drg, sd_drg) 
  D_drg[i] <- ks.test(Y,pnorm,mean_drg, sd_drg, exact=TRUE)$statistic
}

dn_drg <- ks.test(r_drg, pnorm, mean_drg ,sd_drg ,exact=TRUE)$statistic
hist(D_drg,prob=T)
points(dn_drg,0,pch=19,col=2)

p_value_drg <- length(D_drg[D_drg>dn_drg])/N; p_value_drg


# Spółka II

y_swg <- log(kurs_zamkniecia_swg)
r_swg <- diff(y_swg)
plot(r_swg)
hist(r_swg, probability = T)
fnorm_r_swg <- fitdist(r_swg, "norm")

#MC

mean_swg <- fnorm_r_swg$estimate[1]
sd_swg <- fnorm_r_swg$estimate[2]

N <- 1000
n <- length(r_swg)

D_swg <- c()


for (i in 1:N) { 
  
  Y <- rnorm(n, mean_swg, sd_swg) 
  D_swg[i] <- ks.test(Y,pnorm,mean_swg, sd_swg, exact=TRUE)$statistic
}

dn_swg <- ks.test(r_swg, pnorm, mean_swg ,sd_swg ,exact=TRUE)$statistic
hist(D_swg,prob=T)
points(dn_swg,0,pch=19,col=2)

p_value_swg <- length(D_swg[D_swg>dn_swg])/N; p_value_swg


denscomp(fnorm_r_drg)
qqcomp(fnorm_r_drg)
cdfcomp(fnorm_r_drg)
ppcomp(fnorm_r_drg)
gofstat(fnorm_r_drg)

denscomp(fnorm_r_swg)
qqcomp(fnorm_r_swg)
cdfcomp(fnorm_r_swg)
ppcomp(fnorm_r_swg)
gofstat(fnorm_r_swg)


combined_data <- merge(ccc_d, ccc_s, by = "Data")
kursy <- combined_data[,c(5,10)]



p <-  ggplot(kursy, aes(x=Zamkniecie.x, y=Zamkniecie.y)) + geom_point()
p
ggMarginal(p, type="histogram")

mu <- colMeans(kursy); mu

Sigma <- cov(kursy)             #estymator nieobciazony

n <- dim(kursy)[1]; n
Sigma_ob <- (n-1)*cov(kursy)/n  #estymator obciążony

Sigma; Sigma_ob

P <- cor(kursy)  #macierz korelacji
P


#generujemy probe z rozkladu N(mu,Sigma)
n <- nrow(kursy); n

set.seed(100)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)
#wykresy rozrzutu
par(mfrow=c(1,2))
plot(kursy, xlim=c(1.3,2.8),ylim=c(10,23))
plot(Z,xlim=c(1.3,2.8),ylim=c(10,23))


# Utwórz siatkę punktów
x <- seq(min(mu[1] - 3 * sqrt(Sigma[1, 1])), max(mu[1] + 3 * sqrt(Sigma[1, 1])), length.out = 100)
y <- seq(min(mu[2] - 3 * sqrt(Sigma[2, 2])), max(mu[2] + 3 * sqrt(Sigma[2, 2])), length.out = 100)
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)


# Narysuj wykres 3D
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")

gestosc_d <- dnorm(x,mean=mu[1],sd=sd(diff(log(ccc_d$Zamkniecie))))
gestosc_s <- dnorm(x,mean=mu[1],sd=sd(diff(log(ccc_d$Zamkniecie))))

plot(x, gestosc_d, type="l")
plot(y, gestosc_s, type="l")
