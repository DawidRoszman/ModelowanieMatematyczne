#http://drageusgames.com/
install.packages("fitdistrplus")
library(fitdistrplus)

ccc_d <- read.csv("C:/Users/dawid/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")
ccc_d <- read.csv("D:/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")

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


