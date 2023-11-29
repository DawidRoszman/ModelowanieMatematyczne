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


