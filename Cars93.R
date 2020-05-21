install.packages('MASS')
install.packages('e1071')
library(MASS)
data(Cars93)
View(Cars93)

#Okreœlenie które ze zmiennych w Cars93 maj¹ charakter iloœciowy, a które jakoœciowy
str(Cars93)
#zmiennne iloœciowe to : 
#Min.Price, Price ,Max.Price, MPG.city ,MPG.highway ,EngineSize ,Horsepower, RPM ,Rev.per.mile, Fuel.tank.capacity ,Passengers
#Length, Wheelbase, Width, Turn.circle ,Rear.seat.room, Luggage.room, Weight

#zmienne jakoœciowe :
#Manufacturer,Model,Type,AirBags,DriveTrain,Cylinders,Man.trans.avail,Origin,Make

#Wykres s³upkowy dla zmiennej Cylinders
barplot(table(Cars93$Cylinders))

#Wyliczenie statystyk próbkowych oraz ich interpretacja
library(e1071)
konie_mechaniczne <- Cars93$Horsepower
mean(konie_mechaniczne) #Œredni iloœæ koni mechanicznych 143.828
sd(konie_mechaniczne) # Odchylenie standardowe koni mechanicznych od wartoœci œredniej 52.37441
median(konie_mechaniczne) # Wartoœæ œrodkowa (mediana,Q2) koni mechanicznych 140
quantile(konie_mechaniczne,0.25)# Q1 oznacza ¿e 25% aut ma  mniej ni¿ 103 konie mechaniczne.
quantile(konie_mechaniczne,0.75)# Q2 oznacza ¿e 75% aut ma mniej ni¿ 170 konie mechaniczne.
IQR(konie_mechaniczne)#Jest to ró¿nica miêdzy 1 a 3 kwartylem     67
skewness(konie_mechaniczne)# 0.9212474
#Skoœnoœæ prawo strona gdy¿ Mediana < Œredniej i skoœnoœæ > 0 
kurtosis(konie_mechaniczne) #0.9029132
# rozk³ad wysmuk³y K > 0 czyli wartoœci cechy bardziej skoncentrowane ni¿ przy rozk³adzie normalnym.

#Wyznaczenie wartoœci odstaj¹cyh i interpretacja
konie_mechaniczne <- Cars93$Horsepower
UL <- quantile(konie_mechaniczne,0.75)+1.5*IQR(konie_mechaniczne) #D³ugoœæ prawego w¹s¹
sum(konie_mechaniczne > UL)# suma wartoœci odstaj¹cych
konie_mechaniczne[which(konie_mechaniczne > UL)]#Wartoœci odstaj¹ce: 295,300,300,278
LL <- quantile(konie_mechaniczne, 0.25) - 1.5*IQR(konie_mechaniczne)#D³ugoœæ lewego w¹s¹
sum(konie_mechaniczne < LL)# suma wartoœci odstaj¹cych
konie_mechaniczne[which(konie_mechaniczne < LL)]# Brak wartoœci odstaj¹cych
boxplot(konie_mechaniczne)

#Dla wartoœci Horsepower wystêpuj¹ obserwacje odstaj¹ce s¹ to:295,300,300,278 
# s¹ tak¿e ukazane u góry wykresu

#Wykonanie wykresu skrzynkowego dla Horsepower w zale¿noœci od rodzaju napêdu
boxplot(Cars93$Horsepower~Cars93$DriveTrain,col=rainbow(3),xlab = 'DriveTrain',ylab = 'Horsepower', main = 'Wykres Skrzynkowy')
#Najmniejszy rozstêp wystêpuje w autach 4WD co oznacza ¿e te samochody maj¹ zbli¿on¹
#moc koni mechanicznych.
#Najwiêkszy rozstêp wystepuje w Rear co oznacza ¿e maj¹ bardzo zró¿nicowan¹ wartoœæ koni mechanicznych
#Najwiêksz¹ moc koni mechanicznych maj¹ auta Rear
#4WD i Front posiadaj¹ po 1 wartoœci odstaj¹cej, która znacznie ró¿ni siê od pozosta³ych.

#Wyznaczenie 99% przedzia³u ufnoœci dla odchylenie standardowego Wheelbase
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


#Sprawdzenie czy rozk³ad zu¿ycia paliwa MPG.highway dla aut które nie pochodz¹ z USA i maj¹ pojemnoœæ <= 2 
#oraz weryfikuje hipotezê na poziomie istotnoœci 0,05
auta <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize <= 2]

# test Shapiro-Wilka
# H0: X~Czy rozk³ad spalania w je¿dzie pozamiejskiej jest normalny
# H1: X nie ma rozk³adu normalnego

#alpha = 0.05
shapiro.test(auta)
#p-value = 0.1598 > alpha = 0.05 >>> X~N
#zatem X ma rozk³ad normalny.



#Sprawdzenie czy wariancja zu¿ycia paliwa MPG.highway dla samochodów, które nie pochodz¹ z USA 
#o pojemnoœciach silnika do 2 litrów i powy¿ej 2 litrów ró¿ni¹ siê istotnie
#oraz weryfikuje hipotezê na poziomie istotnoœci 0,05
auta_powyzej <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize > 2]
auta_ponizej <-Cars93$MPG.highway[Cars93$Origin == 'non-USA' & Cars93$EngineSize < 2]

#H0: var(auta_ponizej) = var(auta_powyzej)
#H1: var(auta_ponizej) != var(auta_powyzej)

#Sprawdzenie czy auta_powyzej i auta_ponizej maj¹ rozk³ad normalny:

# H0: auta_powyzej~normalny
# H1: auta_powyzej nie ma rozk³adu normalnego
shapiro.test(auta_powyzej)
# p-value = 0.4423 > 0.05 czyli auta_powyzej ~ N

# H0: auta_ponizej~normalny
# H1: auta_ponizej nie ma rozk³adu normalnego
shapiro.test(auta_ponizej)
# p-value =  0.1903 > 0.05 czyli auta_ponizej ~ N

var.test(auta_ponizej,auta_powyzej)
#p-value = 0.009391 < 0.05 czyli odrzucamy H0 i przyjmujemy H1
# zatem wariacje ró¿ni¹ siê istotnie


#Sprawdzenie czy zu¿ycie paliwa w jeŸdzie MPG.highway dla samochodów, które nie pochodz¹ z USA 
# i czy samochody o pojemnoœci do 2 litrów maj¹ istotnie ni¿sze zu¿ycie paliwa ni¿ samochody o pojemnoœci silnika wiêkszej ni¿ 2 litry

# Z zadania wy¿ej wiemy ¿e auta_ponizej i auta_powyzej maja rozklad normalny
#H0: mean(auta_ponizej) = mean(auta_powyzej)
#H1: mean(auta_ponizej) < mean(auta_powyzej)
#alpha = 0.05
t.test(auta_powyzej,auta_ponizej,alternative = 'g',conf.level = 0.95)
#p-value = 1 > alpha = 0.05 czyli zostajemy przy H0
# Czyli samochody nie maj¹ znacznie istotnie ni¿szego zu¿ycia paliwa


#Wyliczenie	czy œrednia ró¿nica pomiêdzy zu¿yciem paliwa w jeŸdzie MPG.highway i MPG.city dla wszystkich aut jest istotnie wiêksza od 5 mil/galon 
#sprawdzamy czy jest rozk³ad normalny
roznica_spalania <- (Cars93$MPG.highway - Cars93$MPG.city)
# test Shapiro-Wilka
# H0: roznica_spalania ~ ma rozk³ad normalny
# H1: ~H0
shapiro.test(roznica_spalania)
#p-value = 0.003447 < alpha = 0.05
# czyli odrzucamy H0 a przyjmuejmy a H1. Roznica spalania nie ma rozk³adu normalnego


# n > 25, rozk³ad nieznany 
#H0: roznica_spalanie_auta = 5
#H1: roznica_spalanie_auta > 5
t.test(roznica_spalania,mu=5,alternative = 'g')
#p-value = 1.923e-14 <  alpha = 0.05 czyli odrzucamy H0 a przyjmujemy H1
# zatem srednia roznica pomiedzy zuzyzciem paliwa w jezdzie pozamiejskiej i miejskiej
# jest istotnie wieksza

#Badamy czy dany odsetek samochodów pochodz¹cych z USA  wœród aut o 4 cylindrach i wœród aut o 6 cylindrach ró¿ni siê istotnie 
n1 <- length(Cars93$Manufacturer[Cars93$Cylinders == 4 ]) #liczebnoœæ próby = auta o 4 cylindach
k1 <- length(Cars93$Manufacturer[Cars93$Cylinders == 4 & Cars93$Origin == 'USA']) #liczba sukcesów = auta o 4 cylindach i z USA

n2 <- length(Cars93$Manufacturer[Cars93$Cylinders == 6 ]) #liczebnoœæ próby = auta o 6 cylindach
k2 <- length(Cars93$Manufacturer[Cars93$Cylinders == 6 & Cars93$Origin == 'USA']) #liczba sukcesów = auta o 6 cylindach i z USA

# p1 = k1/n1
# p2 = k2/n2

# H0: p1 = p2
# H1: p1 != p2

prop.test(c(k1,k2), c(n1,n2))

# p-value = 0.1383 > alpha(alpha = 0.05) czyli zostajemy przy H0
# na poziomie istotnoœci 0.05 nie mo¿emy stwoierdziæ, ¿e odsetki te ró¿ni¹ siê istotnie

#Wyliczenie 90% przedzia³ ufnoœci dla œredniej liczby obrotów na minutê przy maksymalnej mocy dla wszystkich aut 
# X -iloœæ obrotów na minutê przy maksymalnej mocy
# X~nieznany, n=93  >> model 3 dla œredniej
t.test(Cars93$RPM, conf.level = 0.90)$conf.int
# [5177.829, 5383.461]


# mamy 90% pewnoœci ¿e œrednia iloœæ obrotów na minutê przy maksymalnej mocy jest z przedzia³u 
# [5177.829, 5383.461]

