#http://drageusgames.com/
ccc_d <- read.csv("C:/Users/dawid/Code/UG/ModelowanieMatematyczne/ZadanieProjektowe/drg_d.csv")

class(ccc_d)

kurs_zamkniecia <- ccc_d$Zamkniecie
data <- as.Date(ccc_d$Data)

plot(data, kurs_zamkniecia)

hist(kurs_zamkniecia, probability = TRUE)

sd(kurs_zamkniecia)

