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
ccc_d <- read.csv("~/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_s <- read.csv("~/ModelowanieMatematyczne/ZadanieProjektowe/swg_d.csv")
ccc_d <- read.csv("D:/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_d <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
kurs_zamknieca_drg <- ccc_d$Zamkniecie

ccc_s <- read.csv("~/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/swg_d.csv")

#1

kurs_zamkniecia_swg <- ccc_s$Zamkniecie


data <- as.Date(ccc_d$Data)

plot(data, kurs_zamknieca_drg, xlab = "Data", ylab = "Kurs zamknięcia")

hist(kurs_zamknieca_drg, probability = TRUE, xlab = "Kurs zamknięcia", main = "Histogram kursów zamknięcia")


#2

sd(kurs_zamknieca_drg)

descdist(kurs_zamknieca_drg)

#3

fnorm <- fitdist(kurs_zamknieca_drg, "norm")
fln <- fitdist(kurs_zamknieca_drg, "lnorm")
fg <- fitdist(kurs_zamknieca_drg, "gamma")

fnorm
fln
fg

#4

denscomp(list(fnorm, fln, fg))
qqcomp(list(fnorm, fln, fg))
cdfcomp(list(fnorm, fln, fg))
ppcomp(list(fnorm, fln, fg))


gofstat(list(fnorm, fln, fg))

#5

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
ppcomp(list(fexp,fln,fnorm,fgamma),legenttext =plot.legend)

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
combined_data <- merge(ccc_d, ccc_s, by = "Data")
kursy <- combined_data[,c(5,10)]

# Spółka I DRG
y_drg <- log(kursy$Zamkniecie.x)
r_drg <- diff(y_drg)
plot(r_drg)
hist(r_drg, probability = T)
fnorm_r_drg <- fitdist(r_drg, "norm")

denscomp(fnorm_r_drg)
qqcomp(fnorm_r_drg)
cdfcomp(fnorm_r_drg)
ppcomp(fnorm_r_drg)

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

y_swg <- log(kursy$Zamkniecie.y)
r_swg <- diff(y_swg)
plot(r_swg)
hist(r_swg, probability = T)
fnorm_r_swg <- fitdist(r_swg, "norm")

denscomp(fnorm_r_swg)
qqcomp(fnorm_r_swg)
cdfcomp(fnorm_r_swg)
ppcomp(fnorm_r_swg)

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

#B

log_kursy = data.frame(r_drg, r_swg)


p <-  ggplot(log_kursy, aes(x=r_drg, y=r_swg)) + geom_point()
p
ggMarginal(p, type="histogram")

mu <- colMeans(log_kursy); mu

Sigma <- cov(log_kursy)             #estymator nieobciazony

n <- dim(kursy)[1]; n
Sigma_ob <- (n-1)*cov(log_kursy)/n  #estymator obciążony

Sigma; Sigma_ob

P <- cor(log_kursy)  #macierz korelacji
P


#generujemy probe z rozkladu N(mu,Sigma)
n <- nrow(log_kursy); n

set.seed(100)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)
#wykresy rozrzutu
par(mfrow=c(1,2))
plot(log_kursy, main = "Dane orginalne")
plot(Z, main = "Dane wygenerowane")


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

plot(x, gestosc_d, type="l", xlim=c(-0.20,0.20 ))
plot(y, gestosc_s, type="l", xlim=c(-0.20,0.20 ))
class(log_kursy)
head(log_kursy)
ggplot(log_kursy, aes(x=r_drg, y=r_swg)) + geom_point() 

#Dodatkowe

h <- lm(log_kursy$r_drg~log_kursy$r_swg, data=log_kursy)
h
sum <- summary(h)
sum
reszty <- h$residuals

#histogram i qq-ploty
hist(reszty)

qqnorm(reszty)
qqline(reszty,col=2)


# Projekt 3

# Model regresji liniowej
model_lm <- lm(log_kursy$r_drg ~ log_kursy$r_swg, data = log_kursy)

# Reszty modelu
reszty <- residuals(model_lm); reszty; mean(reszty)

hist(reszty, main = "Histogram reszt modelu", xlab = "Reszty modelu")
qqnorm(reszty)
qqline(reszty, col = 2)

plot(log_kursy$r_drg, log_kursy$r_swg, main = "Model regresji liniowej", xlab = "Różnica log(kursu) DRG", ylab = "Różnica log(kursu) SWG")
abline(model_lm, col = "red")

summary(model_lm)

RSS <- sum(residuals(model_lm)^2); RSS

# Uproszczony model

uproszczony_model_b0 <- lm(r_drg ~ 1, data = df)  # Prosty model bez r_swg

plot(df$r_drg, df$r_swg, main = "Uproszczony Model bez r_swg (b0)", xlab = "r_drg", ylab = "r_swg")
abline(uproszczony_model_b0, col = "red")

summary(uproszczony_model)

# Predykcja

set.seed(100)
tren.indeksy <- sample(1:nrow(df), 0.8*nrow(df)) 

tren.dane <- df[tren.indeksy, ] 
test.dane <- df[-tren.indeksy, ]

dist.lm <- lm(r_drg ~ r_swg, data=tren.dane)  
dist.lm$coefficients

dist.pred <- predict(dist.lm, test.dane) 

dist.h <- data.frame(cbind(real=test.dane$r_drg, pred=dist.pred))
qqplot(dist.h$real, dist.h$pred, col = "blue", main = "Q-Q Plot", xlab = "Teoretyczne kwantyle", ylab = "Obserwowane kwantyle")

abline(a = 0, b = 1, col = "red", lty = 2)

# Prosta: R2 = 0.003916 - 0.003338 * R1
# Współczynnik determinacji: 3.781e-05
# ε ∼ N(0, σ2): -3.445358e-19
# RSS: 0.122421
# predykcja_r2: 0.003918072 
