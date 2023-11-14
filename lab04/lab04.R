#http://drageusgames.com/
ccc_d <- read.csv("~/Code/UG/ModelowanieMatematyczne/lab04/drg_d.csv")

class(ccc_d)


kurs_otw <- ccc_d$Otwarcie
kurs_zam <- ccc_d$Zamkniecie

# 1. Narysuj wykres kursu otwarcia i zamknięcia (na jednym wykresie)
# 2. Zrób histogramy
# 3. Wylicz średni kurs, wariancję, odchylenie standardowe

plot(kurs_otw, type="l", col="red")
lines(kurs_zam, col="blue")

hist(kurs_otw, prob=TRUE)
hist(kurs_zam, prob=TRUE)

mean(kurs_otw)
mean(kurs_zam)

var(kurs_zam)

sqrt(var(kurs_zam))
