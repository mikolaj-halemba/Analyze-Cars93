install.packages('MASS')
install.packages('e1071')
library(MASS)
data(Cars93)
View(Cars93)

#Okre�lenie kt�re ze zmiennych w Cars93 maj� charakter ilo�ciowy, a kt�re jako�ciowy
str(Cars93)
#zmiennne ilo�ciowe to : 
#Min.Price, Price ,Max.Price, MPG.city ,MPG.highway ,EngineSize ,Horsepower, RPM ,Rev.per.mile, Fuel.tank.capacity ,Passengers
#Length, Wheelbase, Width, Turn.circle ,Rear.seat.room, Luggage.room, Weight

#zmienne jako�ciowe :
#Manufacturer,Model,Type,AirBags,DriveTrain,Cylinders,Man.trans.avail,Origin,Make

#Wykres s�upkowy dla zmiennej Cylinders
barplot(table(Cars93$Cylinders))

#Wyliczenie statystyk pr�bkowych oraz ich interpretacja
library(e1071)
konie_mechaniczne <- Cars93$Horsepower
mean(konie_mechaniczne) #�redni ilo�� koni mechanicznych 143.828
sd(konie_mechaniczne) # Odchylenie standardowe koni mechanicznych od warto�ci �redniej 52.37441
median(konie_mechaniczne) # Warto�� �rodkowa (mediana,Q2) koni mechanicznych 140
quantile(konie_mechaniczne,0.25)# Q1 oznacza �e 25% aut ma  mniej ni� 103 konie mechaniczne.
quantile(konie_mechaniczne,0.75)# Q2 oznacza �e 75% aut ma mniej ni� 170 konie mechaniczne.
IQR(konie_mechaniczne)#Jest to r�nica mi�dzy 1 a 3 kwartylem     67
skewness(konie_mechaniczne)# 0.9212474
#Sko�no�� prawo strona gdy� Mediana < �redniej i sko�no�� > 0 
kurtosis(konie_mechaniczne) #0.9029132
# rozk�ad wysmuk�y K > 0 czyli warto�ci cechy bardziej skoncentrowane ni� przy rozk�adzie normalnym.

#Wyznaczenie warto�ci odstaj�cyh i interpretacja
konie_mechaniczne <- Cars93$Horsepower
UL <- quantile(konie_mechaniczne,0.75)+1.5*IQR(konie_mechaniczne) #D�ugo�� prawego w�s�
sum(konie_mechaniczne > UL)# suma warto�ci odstaj�cych
konie_mechaniczne[which(konie_mechaniczne > UL)]#Warto�ci odstaj�ce: 295,300,300,278
LL <- quantile(konie_mechaniczne, 0.25) - 1.5*IQR(konie_mechaniczne)#D�ugo�� lewego w�s�
sum(konie_mechaniczne < LL)# suma warto�ci odstaj�cych
konie_mechaniczne[which(konie_mechaniczne < LL)]# Brak warto�ci odstaj�cych
boxplot(konie_mechaniczne)

#Dla warto�ci Horsepower wyst�puj� obserwacje odstaj�ce s� to:295,300,300,278 
# s� tak�e ukazane u g�ry wykresu

#Wykonanie wykresu skrzynkowego dla Horsepower w zale�no�ci od rodzaju nap�du
boxplot(Cars93$Horsepower~Cars93$DriveTrain,col=rainbow(3),xlab = 'DriveTrain',ylab = 'Horsepower', main = 'Wykres Skrzynkowy')
#Najmniejszy rozst�p wyst�puje w autach 4WD co oznacza �e te samochody maj� zbli�on�
#moc koni mechanicznych.
#Najwi�kszy rozst�p wystepuje w Rear co oznacza �e maj� bardzo zr�nicowan� warto�� koni mechanicznych
#Najwi�ksz� moc koni mechanicznych maj� auta Rear
#4WD i Front posiadaj� po 1 warto�ci odstaj�cej, kt�ra znacznie r�ni si� od pozosta�ych.

#Wyznaczenie 99% przedzia�u ufno�ci dla odchylenie standardowego Wheelbase
rozmiar_kol ~ nieznany rozklad

rozmiar_kol <- Cars93$Wheelbase
n <- length(rozmiar_kol)
srednia_kol <- mean(rozmiar_kol)
S <- sqrt(sum((rozmiar_kol-srednia_kol)^2)/(n-1))

Odchylenie_Stand <- function(n, S, alpha=0.01){
  
  L <- sqrt(2*n-2)*S/(sqrt(2*n-3)+qnorm(1-alpha/2))
  P <- sqrt(2*n-2)*S/(sqrt(2*n-3)-qnorm(1-alpha/2))
  c(L, P) 
}


#Sprawdzenie czy rozk�ad zu�ycia paliwa MPG.highway dla aut kt�re nie pochodz� z USA i maj� pojemno�� <= 2 
#oraz weryfikuje hipotez� na poziomie istotno�ci 0,05
auta <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize <= 2]

# test Shapiro-Wilka
# H0: X~Czy rozk�ad spalania w je�dzie pozamiejskiej jest normalny
# H1: X nie ma rozk�adu normalnego

#alpha = 0.05
shapiro.test(auta)
#p-value = 0.1598 > alpha = 0.05 >>> X~N
#zatem X ma rozk�ad normalny.



#Sprawdzenie czy wariancja zu�ycia paliwa MPG.highway dla samochod�w, kt�re nie pochodz� z USA 
#o pojemno�ciach silnika do 2 litr�w i powy�ej 2 litr�w r�ni� si� istotnie
#oraz weryfikuje hipotez� na poziomie istotno�ci 0,05
auta_powyzej <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize > 2]
auta_ponizej <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize < 2]

#H0: var(auta_ponizej) = var(auta_powyzej)
#H1: var(auta_ponizej) != var(auta_powyzej)

#Sprawdzenie czy auta_powyzej i auta_ponizej maj� rozk�ad normalny:

# H0: auta_powyzej~normalny
# H1: auta_powyzej nie ma rozk�adu normalnego
shapiro.test(auta_powyzej)
# p-value = 0.4423 > 0.05 czyli auta_powyzej ~ N

# H0: auta_ponizej~normalny
# H1: auta_ponizej nie ma rozk�adu normalnego
shapiro.test(auta_ponizej)
# p-value =  0.1903 > 0.05 czyli auta_ponizej ~ N

var.test(auta_ponizej,auta_powyzej)
#p-value = 0.009391 < 0.05 czyli odrzucamy H0 i przyjmujemy H1
# zatem wariacje r�ni� si� istotnie


#Sprawdzenie czy zu�ycie paliwa w je�dzie MPG.highway dla samochod�w, kt�re nie pochodz� z USA 
# i czy samochody o pojemno�ci do 2 litr�w maj� istotnie ni�sze zu�ycie paliwa ni� samochody o pojemno�ci silnika wi�kszej ni� 2 litry

# Z zadania wy�ej wiemy �e auta_ponizej i auta_powyzej maja rozklad normalny
#H0: mean(auta_ponizej) = mean(auta_powyzej)
#H1: mean(auta_ponizej) < mean(auta_powyzej)
#alpha = 0.05
t.test(auta_powyzej,auta_ponizej,alternative = 'g',conf.level = 0.95)
#p-value = 1 > alpha = 0.05 czyli zostajemy przy H0
# Czyli samochody nie maj� znacznie istotnie ni�szego zu�ycia paliwa


#Wyliczenie	czy �rednia r�nica pomi�dzy zu�yciem paliwa w je�dzie MPG.highway i MPG.city dla wszystkich aut jest istotnie wi�ksza od 5 mil/galon 
#sprawdzamy czy jest rozk�ad normalny
roznica_spalania <- (Cars93$MPG.highway - Cars93$MPG.city)
# test Shapiro-Wilka
# H0: roznica_spalania ~ ma rozk�ad normalny
# H1: ~H0
shapiro.test(roznica_spalania)
#p-value = 0.003447 < alpha = 0.05
# czyli odrzucamy H0 a przyjmuejmy a H1. Roznica spalania nie ma rozk�adu normalnego


# n > 25, rozk�ad nieznany 
#H0: roznica_spalanie_auta = 5
#H1: roznica_spalanie_auta > 5
t.test(roznica_spalania,mu=5,alternative = 'g')
#p-value = 1.923e-14 <  alpha = 0.05 czyli odrzucamy H0 a przyjmujemy H1
# zatem srednia roznica pomiedzy zuzyzciem paliwa w jezdzie pozamiejskiej i miejskiej
# jest istotnie wieksza

#Badamy czy dany odsetek samochod�w pochodz�cych z USA  w�r�d aut o 4 cylindrach i w�r�d aut o 6 cylindrach r�ni si� istotnie 
n1 <- length(Cars93$Manufacturer[Cars93$Cylinders == 4 ]) #liczebno�� pr�by = auta o 4 cylindach
k1 <- length(Cars93$Manufacturer[Cars93$Cylinders == 4 & Cars93$Origin == 'USA']) #liczba sukces�w = auta o 4 cylindach i z USA

n2 <- length(Cars93$Manufacturer[Cars93$Cylinders == 6 ]) #liczebno�� pr�by = auta o 6 cylindach
k2 <- length(Cars93$Manufacturer[Cars93$Cylinders == 6 & Cars93$Origin == 'USA']) #liczba sukces�w = auta o 6 cylindach i z USA

# p1 = k1/n1
# p2 = k2/n2

# H0: p1 = p2
# H1: p1 != p2

prop.test(c(k1,k2), c(n1,n2))

# p-value = 0.1383 > alpha(alpha = 0.05) czyli zostajemy przy H0
# na poziomie istotno�ci 0.05 nie mo�emy stwoierdzi�, �e odsetki te r�ni� si� istotnie

#Wyliczenie 90% przedzia� ufno�ci dla �redniej liczby obrot�w na minut� przy maksymalnej mocy dla wszystkich aut 
# X -ilo�� obrot�w na minut� przy maksymalnej mocy
# X~nieznany, n=93  >> model 3 dla �redniej
t.test(Cars93$RPM, conf.level = 0.90)$conf.int
# [5177.829, 5383.461]


# mamy 90% pewno�ci �e �rednia ilo�� obrot�w na minut� przy maksymalnej mocy jest z przedzia�u 
# [5177.829, 5383.461]

