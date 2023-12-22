#http://drageusgames.com/
library(fitdistrplus)
library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

ccc_d <- read.csv("D:/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_d <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
kurs_zamknieca_drg <- ccc_d$Zamkniecie

ccc_s <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/swg_d.csv")
kurs_zamkniecia_swg <- ccc_s$Zamkniecie

data <- as.Date(ccc_d$Data)
?plot
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

mean_swg <- rnorm_swg$estimate[1]
sd_swg <- rnorm_swg$estimate[2]

N <- 1000
n <- length(r2)

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
