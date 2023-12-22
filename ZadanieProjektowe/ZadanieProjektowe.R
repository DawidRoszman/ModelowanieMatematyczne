#http://drageusgames.com/
install.packages("fitdistrplus")
library(fitdistrplus)

ccc_d <- read.csv("D:/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_d <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
class(ccc_d)

kurs_zamkniecia <- ccc_d$Zamkniecie
data <- as.Date(ccc_d$Data)
?plot
plot(data, kurs_zamkniecia, xlab = "Data", ylab = "Kurs zamknięcia")

hist(kurs_zamkniecia, probability = TRUE, xlab = "Kurs zamknięcia", main = "Histogram kursów zamknięcia")

sd(kurs_zamkniecia)

descdist(kurs_zamkniecia)


fnorm <- fitdist(kurs_zamkniecia, "norm")
fln <- fitdist(kurs_zamkniecia, "lnorm")
fg <- fitdist(kurs_zamkniecia, "gamma")

fnorm
fln
fg

denscomp(list(fnorm, fln, fg))
qqcomp(list(fnorm, fln, fg))
cdfcomp(list(fnorm, fln, fg))
ppcomp(list(fnorm, fln, fg))

gofstat(list(fnorm, fln, fg))

N <- 1000
n <- length(kurs_zamkniecia)

D <- c()

shape <- fg$estimate[[1]]
rate <- fg$estimate[[2]]

for (i in 1:N) { 
  
  Y <- rgamma(n, shape, rate) 
  D[i] <- ks.test(Y,pgamma,shape = shape, rate = rate,exact=TRUE)$statistic
}

dn <- ks.test(kurs_zamkniecia,pgamma,shape = shape, rate = rate,exact=TRUE)$statistic
hist(D,prob=T)
points(dn,0,pch=19,col=2)

p_value <- length(D[D>dn])/N; p_value

alpha <- 0.05
p_value <= alpha

# PART II




# Spółka I
y <- log(kurs_zamkniecia)
r <- diff(y)
plot(r)
plot(kurs_zamkniecia)
hist(r, probability = T)
fnorm_r <- fitdist(r, "norm")

# MC

mean <- fnorm_r$estimate[1]
sd <- fnorm_r$estimate[2]

N <- 1000
n <- length(r)

D <- c()

for (i in 1:N) { 
  
  Y <- rnorm(n, mean, sd) 
  D[i] <- ks.test(Y,pnorm,mean, sd, exact=TRUE)$statistic
}

dn <- ks.test(r, pnorm, mean ,sd ,exact=TRUE)$statistic
hist(D,prob=T)
points(dn,0,pch=19,col=2)

p_value <- length(D[D>dn])/N; p_value


# Spółka II
ccc_s <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/swg_d.csv")
kurs_zamkniecia2 <- ccc_s$Zamkniecie
y2 <- log(kurs_zamkniecia2)
r2 <- diff(y2)
plot(r2)
hist(r2, probability = T)
rnorm2 <- fitdist(r2, "norm")

#MC

mean2 <- rnorm2$estimate[1]
sd2 <- rnorm2$estimate[2]

N <- 1000
n <- length(r2)

D <- c()


for (i in 1:N) { 
  
  Y <- rnorm(n, mean2, sd2) 
  D[i] <- ks.test(Y,pnorm,mean2, sd2, exact=TRUE)$statistic
}

dn <- ks.test(r2, pnorm, mean2 ,sd2 ,exact=TRUE)$statistic
hist(D,prob=T)
points(dn,0,pch=19,col=2)

p_value <- length(D[D>dn])/N; p_value

denscomp(rnorm)
qqcomp(rnorm)
cdfcomp(rnorm)
ppcomp(rnorm)
gofstat(rnorm)

denscomp(rnorm2)
qqcomp(rnorm2)
cdfcomp(rnorm2)
ppcomp(rnorm2)
gofstat(rnorm2)

combined_data <- merge(ccc_d, ccc_s, by = "Data")
kursy <- combined_data[,c(5,10)]
install.packages("mnormt")

library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

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
