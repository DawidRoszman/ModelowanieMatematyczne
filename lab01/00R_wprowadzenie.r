#R - podstawowe operacje
#=======================
#http://cran.r-project.org/
#https://rstudio.com/
  
#============================
#============================ ZADANIE A (wykonaj wszystkie ponizsze cwiczenia)
#Więcej o każdej funkcj po napisaniu w konsoli
#np. ?ceiling; ?floor; ... itd

#Ctrl L - czysci konsole

#============== Liczby - zaokraglenia
#Cwiczenie 1.  Sprawdz dzialanie ponizszych  funkcji dla x=0.6789
x  <- 0.6789

ceiling(1:5)
floor(x)
trunc(x, ...)
round(x, digits = 2)

#============== Wektory
#Cwiczenie 2. Sprawdz na przykladach dzialanie operatora ":".
1:10;  
10:-10

#Cwiczenie 3. Sprawdz na przykladach dzialanie funkcji "seq".
seq(-10,10, by=2) 
seq(-10,10, length=50)

#Cwiczenie 4. Sprawdz na przykladach dzialanie funkcji "rep".
rep(1:3,5)
rep(1:3,length.out=17)
rep(1:3,rep(5,3))

#Cwiczenie 5. Jak wywolujemy wyrazy wektora. Sprawdz na przykladach.
a=seq(-10,10,by=2); a
a[3]
a[c(4,6,8)]
a[1:5]

#Sprawdz co bedzie jesli w nawiasie jest liczba ujemna. 
#lub operator logiczny.
a[-3]
a[-c(4,6,8)]
a[-(1:5)]
a[a>0]

#Cwiczenie 6. Wykonaj ponizsze operacje.
b=1:10
b[1:5]=0         #wywolaj  wektor b
b[c(1,3,5)]=-1   #wywolaj  wektor b

#Cwiczenie 7. Na wektorach 
x=seq(0,6,by=2)
y=rev(x)             #co robi funkcja "rev"?

#sprawdz dzialania
2-x
x+y
x*y
1/(x+1)+y^2

#Cwiczenie 8.  Dla wektora
x=rep(1:3,2); x
#sprawdz dzialanie funkcji 
max(x)
min(x)
range(x)
length(x)
sort(x)
sum(x)
prod(x)
#Sprawdz tez order(rank), cummax, cummin, which, diff.

#Cwiczenie 9. Dla wektorow
a=c(6,2,4,6,5,3,9,7,3,1,6)
b=c(1,0,8,4,9,1,6,5,7,2,9)
#sprawdz dzialanie ponizszych  funkcji
c(a,b)

pmax(a,b)
pmin(a,b)

X <- rbind(a,b)
Y <- cbind(a,b)
rownames(X) <- c('a','b')
colnames(Y) <-  c('a','b')
head(X); tail(X)
head(Y); tail(Y)

class(X); class(Y)

#Cwiczenie 10. (kombinatoryka)
#liczba permutacji  n elementowych: n!
1*2*3*4*5
factorial(5)

#symbol Newtona - liczba kombinacji k-elementowych ze zbioru n-elementowego
choose(5,2)

#========================= proste wykresy, funkcje: plot, points
#Cwiczenie 10. 
plot(1:10)
points(1:10,rep(4,10), pch=19, col=2)        #dodaje do isniejacego wykresu
points(1:10,rep(2,10), typ="h", col="blue")

#wiecej na temat funkcji 'plot', 'points'  dowiesz sie piszac w konsoli ?plot, ?points 

#========================= proste wykresy, funkcje: curve, abline
#Cwiczenie 11. Narysuj na jednym wykresie wykres funkcji sin(x) i cos(x)

curve(x^2, xlim=c(-4,4), ylim=c(-16,16), 
      xlab=NA, ylab=NA,
      col="blue")
curve(-x^3, col="green", add=T)  #dodajemy wykres do istniejacego
abline(a=0,b=2, col=2)          #dodaje do istniejacego wykresu
points(1,2, pch=19, col=2)
abline(h=0, col="grey")
abline(v=0, col="grey")

#wiecej na temat funkcji 'curve', 'abline' dowiesz sie piszac w konsoli ?curve, ?abline

#========================== wlasne proste funkcje 
my.f <- function(x) {x^2+1}
my.f(2)
my.f(c(2,4))

curve(my.f(x),xlim=c(-5,5))

#---
apply(X,2,my.f) #dziala na kolumnach
apply(X,2,function(x) sum(x))
apply(X,1,max) # dziala na wierszach

#lapply dziala na listach
x <- 1:4
sin(x)
log(x)
exp(x)

#---
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))

#=========================
#========================= ZADANIE B 
#Przeczytaj 00KombPrawdop_slajdy.pdf i zrob znajdujace sie tam przyklady
#wykorzystujac R.

#==================================== 
#Literatura do R np.
#Data Wrangling with R, Springer -- B.C. Borhmke, Springer
#ggplot2, Springer --  Hadley Wickham
