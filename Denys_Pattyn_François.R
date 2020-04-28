#Data inlezen
supermarkt = read.table(file = "supermarkt.txt", header = TRUE,
                        sep = ";", na.strings = "-", dec = ",")

#Totaal in minuten (*24 *60)
supermarkt$t_tot = (supermarkt$t_out - supermarkt$t_in)

#Alle out variabelen verwijderen
supermarkt$t_out = NULL
supermarkt$line_out = NULL
supermarkt$registers_out = NULL
supermarkt$prod_out = NULL
supermarkt$spending_out = NULL
supermarkt$satisfaction_out = NULL

#Categorische veranderlijken maken
as.factor(supermarkt$sex)
as.factor(supermarkt$store)

#Education categorie maken. 
supermarkt$education_cat = cut(supermarkt$education, c(0,3,6,Inf),
                               labels = c("laag","midden","hoog"), 
                               ordered_result = TRUE, include.lowest = TRUE)

supermarkt$t_week = cut(supermarkt$day, c(0,1,4),
                               labels = c("week","weekend"),
                               inlcude.lowest = TRUE)

#Categorische veranderlijken passende naam geven
supermarkt$store = factor(supermarkt$store, levels = c(1,2), labels = c('Brugge', 'Gent'))

supermarkt$day = factor(supermarkt$day, levels = c(1,2,3), labels = c('donderdag', 'vrijdag', 'zaterdag'))

supermarkt$sex = factor(supermarkt$sex, levels = c(1,2), labels = c('man', 'vrouw'))
attach(supermarkt)

####Beschrijvende statistiek
##Kwalitatieve variabelen
#Geslacht
table(sex)
table(sex)/length(sex)
sum(is.na(sex))

#Week
table(t_week)
table(t_week)/length(t_week)
sum(is.na(t_week))

#Categorie education
table(education_cat)
table(education_cat)/length(education_cat)
sum(is.na(education_cat))  

#Store
table(store)
table(store)/length(store)
sum(is.na(store))

##Kwantitatieve variabelen
#age
X = age

hist(age)
sum(is.na(age))
mean(age)
min(age)
max(age)
summary(age)
boxplot(age)
sd(age,na.rm=TRUE)

#Verdeling zoeken
#Controle op normaal en exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X)
sigma = sd(X)
x=seq(min(X),max(X),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')


#Controle op lognormaal
Y = log(age)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')

#Education
X = education
summary(education)
hist(education)
hist(education, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
boxplot(education)
sd(education,na.rm=TRUE)

#Controle op normaal of exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')


#Verwacht aantal producten
X = prod_exp
summary(prod_exp)
hist(prod_exp)
boxplot(prod_exp)
sd(prod_exp,na.rm=TRUE)

#Controle op normaal of exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(prod_exp)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')







r#Verwachte tijd
X = t_exp
summary(t_exp)
hist(t_exp)
hist(t_exp, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75))
boxplot(t_exp)
sd(t_exp,na.rm=TRUE)


#Controle op normaal of exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(t_exp)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')

#Tijd in
X = t_in
summary(t_in)
hist(t_in)
boxplot(t_in)
sd(t_in,na.rm=TRUE)

#Controle op normaal en exponentieel
hist(X, xlab = "Tijdstip binnenkomst", ylab = "Aantal klanten", main ="Histogram t_in")
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(t_in)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')




#Totale tijd
X = t_tot
summary(t_tot)
hist(t_tot)
hist(t_tot, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
boxplot(t_tot)
sd(t_tot,na.rm=TRUE)

#Controle op normaal en exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(t_tot)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')

#Aantal aan kassa
X = line_in
summary(line_in)
hist(line_in)
boxplot(line_in)
sd(line_in,na.rm=TRUE)

#Controle op normaal en exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(line_in)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')

#Aantal open kassa's
X = registers_in
summary(registers_in)
hist(registers_in)
boxplot(registers_in)
hist(registers_in, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))
sd(registers_in,na.rm=TRUE)

#Controle op normaal en exponentieel
hist(X)
hist(X, freq = FALSE)
mu = mean(X, na.rm = TRUE)
sigma = sd(X, na.rm = TRUE)
x=seq(min(X,na.rm = TRUE),max(X, na.rm = TRUE),length=100)
lines(x,dnorm(x,mu,sigma),col='red')
lines(x,dexp(x,1/mu),col='blue')

#Controle op lognormaal
Y = log(registers_in)
hist(Y)
hist(Y, freq = FALSE)
mu = mean(Y, na.rm = TRUE)
sigma = sd(Y, na.rm = TRUE)
y=seq(min(Y, na.rm=TRUE),max(Y, na.rm = TRUE),length=100)
lines(y,dnorm(y,mu,sigma),col='green')


####Inferentiële statistiek
###3.3.1Kenmerken van de steekproef
##Test voor 1 gemiddelde 30 min voor t_tot
#1)Testprobleem:
#H0: mu = 30
#H1: mu =/ 30 --> tweezijdige test
#alfa = 5%
#X = winkeltijd


#2)Teststatistiek
#CLS is ok, want n = 200

#3)Testcriterium AVG, P-waarde

#4)Observaties
t.test(t_tot,mu = 30)
qt(0.025, 169) #-1.9741

#5) Conclusie: We verwerpen H0 niet op significantieniveau alfa = 5%, want P-waarde is groter dan
#0.05, 30 ligt in het BI en t ligt in het AVG (-1.9741 < -0.37787)

##Test voor proportie mannen in steekproef:
#1)Tesprobleem:
#H0: proportie mannen = 49%
#H1: proportie mannen  =/ 49%
#alfa = 5%

#2)Teststatistiek
table(sex)
man = t_tot[ sex == "man"]
length(man) #76


#3)Testcriterium: BI, P-waarde

#4)Observaties
binom.test(76,200,p=0.49, alternative ="two.sided")


#5) Conclusie: We verwerpen H0 op significantieniveau alfa = 5%, aangezien de p-waarde kleiner
#is dan 0.05 en 0.49 niet in het BI ligt.

##Test voor scholingsgraad
scholingsgraad = c(0.35,0.35,0.30)
#1)Tesprobleem:
#H0: Scholingsgraad steekproef volgt de verdeling van de populatie
#H1: Scholingsgraad steekproef volgt de verdeling van de populatie niet
#alfa = 5%

#2)Teststatistiek
#Cochran-regel voldaan
chisq.test(table(education_cat), p = scholingsgraad)$expected

#3) Testcriterium: p-waarde

#4) Geobserveerde waarde:
#We kiezen voor de kruistabel, aangezien de gevens ordinaal zijn en knopen bevatten.
table(education_cat)
chisq.test(table(education_cat), p = scholingsgraad)
chisq.test(table(education_cat), p = scholingsgraad)$observed
chisq.test(table(education_cat), p = scholingsgraad)$residuals

#5) conclusie:  De gegevens wijken sterk af van de hypothetische verdeling. Op significantieniveau
#alfa = 5% verwerpen we H0.





###3.3.2 De gemiddelde winkeltijd
##Verschil tussen verwacht en effectief, gepaarde gegevens:
#1) Testprobleem: 
#H0: gem(verwachte tijd) = gem(effectieve tijd)
#H1: gem(verwachte tijd) =/ gem(effectieve tijd)
#alfa = 5%
#X = verwachte tijd, Y = effectieve tijd

#2) Teststatistiek: gepaarde gegevens, CLS is ok, want n = 199, variantie niet gekend

#3) Testcriterium: AG, P-Waarde

#4) Observaties
x = t_exp
y = t_tot
t.test(x, y, paired = TRUE)
qt(0.025,169) #-1.9741

#5) Conclusie
#We verwerpen H0 op significantieniveau 5%, aangezien 0 niet in het BI zit, de P-waarde kleiner is 
#dan 0.05 en de t-waarde ligt niet in het AVG (-1.9741 > t)

## Verschil tussen mannen en vrouwen
man = t_tot[ sex == "man"]
vrouw = t_tot [sex == "vrouw"]

#1) Testprobleem: 
#H0: gem(tijd man) = gem(tijd vrouw)
#H1: gem(tijd man) =/ gem(tijd vrouw)
#alfa = 5%
#X = totale winkeltijd man, Y = totale winkeltijd vrouw

#2) Teststatistiek: Ongepaarde gegevens, CLS is ok, want n = 200, variantie niet gekend
length(man) #76
length(vrouw) #124

#Normaliteit man en vrouw:
length(man)
qqnorm(man, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot man")
qqline(man,col='red')
hist(man)
length(vrouw)
qqnorm(vrouw, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot vrouw")
qqline(vrouw,col='red')
hist(vrouw)
shapiro.test(man)
shapiro.test(vrouw)
# Niet normaal verdeeld, we verwerpen H0 en kiezen voor de test met ongelijke varianties,
#wegens n groot

#3) Testcriterium: AG, P-Waarde

#4) Observaties
t.test(man, vrouw, paired = FALSE, var.equal = FALSE)
qt(0.025,148) #-1.976122

#5) Conclusies
#We verwerpen H0 niet op significantieniveau alfa = 5%, want 0 ligt in het BI, 
#P-waarde is groter dan 0.05 en t-waarde ligt in AVG (-1.976122 < t)

##Verschil in filiaal
#1) Testprobleem: 
#H0: gem(tijd Brugge) = gem(tijd Gent)
#H1: gem(tijd Brugge) =/ gem(tijd Gent)
#alfa = 5%
# X = tijd in Brugge, Y = tijd in Gent

x = t_tot[store == "Brugge"] #Brugge
y = t_tot[store == "Gent"] #Gent

#2) Teststatistiek: Ongepaarde gegevens, CLS is ok, want n = 200, variantie niet gekend
length(x) #105
length(y) #95

#normaliteit van X en Y
length(x)
qqnorm(x, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot Brugge")
qqline(x,col='red') #Brugge
hist(x)
length(y)
qqnorm(y, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot Gent")
qqline(y,col='red') #Gent 
hist(y)
shapiro.test(x)
shapiro.test(y)
#We verwerpen H0. x en y zijn niet normaal verdeeld. We kiezen nu voor de t-test met ongelijke
#varianties

#3) Testcriterium: AG, P-Waarde

#4) Observaties
t.test(x, y, paired = FALSE, var.equal = FALSE)
qt(0.025,168) #-1.974185

#5) Conclusies
#Op significantieniveau 5% verwerpen we H0 niet, aangezien 0 in het BI ligt en t-waarde in
#het AVG ligt (-1.974185 < 0.18499) en de p-waarde is groter dan 0.05

## verschil tussen week en weekend
#1) Testprobleem: 
#H0: gem(tijd Brugge) = gem(tijd Gent)
#H1: gem(tijd Brugge) =/ gem(tijd Gent)
#alfa = 5%
# X = winkelen in de week, Y = winkelen in het weekend
x = t_tot[t_week == "week"]
y = t_tot[t_week == "weekend"]

#2) Teststatistiek: Ongepaarde gegevens, CLS is ok, want n = 200, variantie niet gekend
length(x) #49
length(y) #151

#normaliteit van X en Y
length(x)
qqnorm(x, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot weekdag")
qqline(x,col='red') #Week
hist(x)
length(y)
qqnorm(y, xlab = "Standaardnormale kwantielen", ylab = "Geordende observaties", 
       main = "Normale kwantielplot weekend")
qqline(y,col='red') #Weekend
hist(y)
shapiro.test(x)
shapiro.test(y)

#3) Testcriterium: AG, P-Waarde

#4) Observaties
t.test(x, y, paired = FALSE, var.equal = FALSE)
qt(0.025,118) #-1.980272

#5) Conclusies
#Op significantieniveau 5% verwerpen we H0, aangezien 0 niet in het BI ligt en de t-waarde niet in
#het AVG ligt ( -5.1564 < -1.980272 ) en de p-waarde is kleiner dan 0.05.


###3.3.3 Associatie van de verschillende veranderlijken met de totale winkeltijd
#Controle normaliteit t_tot
shapiro.test(t_tot)
shapiro.test(log(t_tot))
#T_tot is lognormaal verdeeld, dus om afhankelijkheid te berekenen tussen totale winkeltijd
# en tussen andere veranderlijken zullen we steeds de testen voor niet normaal verdeelde
#veranderlijken moeten hanteren. T_tot is continu dus steeds test voor continue veranderlijken.

## Age
#H0: rho = 0
shapiro.test(age) #Niet normaal verdeeld. Eigenlijk overbodig, wegens t_tot.
cor.test(age, t_tot, method = "spearman") #licht monotoon stijgend verband = H0 verwerpen


## prod_exp
#H0: rho = 0
cor.test(prod_exp, t_tot, method = "spearman") #Monotoon stijgend verband

##t_exp
#H0: rho = 0
cor.test(t_exp, t_tot, method = "spearman") #Monotoon stijgend verband

##t_in
#H0: rho = 0
cor.test(t_in, t_tot, method = "spearman") #licht monotoon dalend verband, want rho is negatief

##line_in
#H0: rho = 0
cor.test(line_in, t_tot, method = "spearman") #Licht monotoon stijgend verband

##registers_in
#H0: rho = 0
cor.test(registers_in, t_tot, method = "spearman") #Licht monotoon stijgend verband

##Education
#H0: rho = 0
cor.test(education, t_tot, method = "spearman") #Licht dalend verband


###3.3.4 Verklaren van de totale winkeltijd
#lineaire versie
plot(t_tot,t_exp)
eenvoudigmodel1 = lm(t_tot ~ t_exp)
summary(eenvoudigmodel1)
plot(t_tot ~ t_exp, xlab= "Verwachte winkeltijd", ylab = "Totale winkeltijd")
abline(eenvoudigmodel1, col = "blue")
# F-test: p-waarde = 1.015e-12, dus kleiner dan 0.05, er is dus wel degelijk significantie
# t-test: 7.716, t-waarde ligt ver van 0, dus H0 wordt verworpen, ver in de rechterstaart
# t_tot = 12.93883 + 0.65072 t_exp

x_i = eenvoudigmodel1$model[,2]
betrouwbaarheids = predict(eenvoudigmodel1,  interval = "confidence", level = 0.95)
predictie = predict(eenvoudigmodel1,  interval = "prediction", level = 0.95)
lines(sort(x_i),betrouwbaarheids[order(x_i),2], col = "red")
lines(sort(x_i),betrouwbaarheids[order(x_i),3], col = "red")
lines(sort(x_i),predictie[order(x_i),2], col = "green")
lines(sort(x_i),predictie[order(x_i),3], col = "green")

title(main = "Lineair eenvoudig model")
legend(43,100, legend = c("Betrouwbaarheidsinterval", "voorspellingsinterval"), col = c("red","green"), lty = 1, cex = 0.8)



#Logaritmische versie
A = log10(t_tot)
plot(A, t_exp)
eenvoudigmodel2 = lm(A ~ t_exp)
summary(eenvoudigmodel2)
plot(A ~ t_exp, xlab= "Verwachte winkeltijd", ylab = "Logaritmische totale winkeltijd")
abline(eenvoudigmodel2, col = "blue")
# F-test: kleine p-waarde dus verwerp H0
# t-test: grote t-waarde van 9.208 dus verwerp H0
# log10(t_tot) = 1.114968 + 0.010929 t_exp

x_i = eenvoudigmodel2$model[,2]
betrouwbaarheids2 = predict(eenvoudigmodel2,  interval = "confidence", level = 0.95)
predictie2 = predict(eenvoudigmodel2,  interval = "prediction", level = 0.95)
lines(sort(x_i),betrouwbaarheids2[order(x_i),2], col = "red")
lines(sort(x_i),betrouwbaarheids2[order(x_i),3], col = "red")
lines(sort(x_i),predictie2[order(x_i),2], col = "green")
lines(sort(x_i),predictie2[order(x_i),3], col = "green")

title(main = "Logaritmisch eenvoudig model", sub = "confidence", col.sub = "Red")

##Op 1 puntenplot

plot(t_tot ~ t_exp)
abline(eenvoudigmodel1, col = "blue")
lines(sort(t_exp), 10^(eenvoudigmodel2$coefficients[1]+eenvoudigmodel2$coefficients[2]*sort(t_exp)), col = "blue", lty = "dashed")

x_i = eenvoudigmodel1$model[,2]
betrouwbaarheids1 = predict(eenvoudigmodel1,  interval = "confidence", level = 0.95)
predictie1 = predict(eenvoudigmodel1,  interval = "prediction", level = 0.95)
lines(sort(x_i),betrouwbaarheids1[order(x_i),2], col = "red")
lines(sort(x_i),betrouwbaarheids1[order(x_i),3], col = "red")
lines(sort(x_i),predictie1[order(x_i),2], col = "green")
lines(sort(x_i),predictie1[order(x_i),3], col = "green")

y_i = eenvoudigmodel2$model[,2]
betrouwbaarheids2 = 10^predict(eenvoudigmodel2,  interval = "confidence", level = 0.95)
predictie2 = 10^predict(eenvoudigmodel2,  interval = "prediction", level = 0.95)
lines(sort(y_i),betrouwbaarheids2[order(x_i),2], col = "red", lty = "dashed")
lines(sort(y_i),betrouwbaarheids2[order(x_i),3], col = "red", lty = "dashed")
lines(sort(y_i),predictie2[order(x_i),2], col = "green", lty = "dashed")
lines(sort(y_i),predictie2[order(x_i),3], col = "green", lty = "dashed")

title(main = "Puntenplot lineair en logaritmisch model")
legend (3,101, legend=c("Betrouwbaarheidsinterval", "Voorspellingsinterval", "Lineair model", "Logaritmisch model"), 
        col=c("red", "green", "black", "black"), lty = 1:2, cex=0.8)

#Meervoudige achterwaartse regressie
winkeltijd1 = lm(t_tot ~ age + education + prod_exp + t_exp + t_in + line_in + registers_in) 
summary(winkeltijd1)

# F-test: p-waarde = 2.566e-13, dus kleiner dan 0.05, er is dus wel degelijk significantie
# t-test: voor de variabelen prod_exp en t_exp ligt de t-waarde ver van 0, 
# dus H0 wordt verworpen, ver in de rechterstaart
# t_tot = 1.1 + 0.076 age - 0.58 education + 0.45 prod_exp + 
# 0.34 t_exp + 4.4 t_in + 0.073 line_in + 1.1 registers_in

#t_in is het minst significant, daarom verwijderen we deze

winkeltijd1 = update(winkeltijd1, .~. - t_in)
summary(winkeltijd1)

#line_in is het minst significant, daarom verwijderen we deze

winkeltijd1 = update(winkeltijd1, .~. - line_in)
summary(winkeltijd1)

#age is het minst significant, daarom verwijderen we deze

winkeltijd1 = update(winkeltijd1, .~. - age)
summary(winkeltijd1)

#education is het minst significant, daarom verwijderen we deze

winkeltijd1 = update(winkeltijd1, .~. - education)
summary(winkeltijd1)
# t_tot = 2.1 + 0.42 prod_exp + 0.37 t_exp + 1.3 registers_in

#De overige variabelen zijn significant genoeg (p-value kleiner dan 0.01)
#We zien wel dat het intercept niet significant is.

#Meervoudige achterwaardse regressie voor logaritme
winkeltijd2 = lm(log10(t_tot) ~ age + education + prod_exp + t_exp + t_in + line_in + registers_in) 
summary(winkeltijd2)

# F-test: p-waarde = 2.2e-16, dus kleiner dan 0.05, er is dus wel degelijk significantie
# t-test: voor de variabelen prod_exp en t_exp ligt de t-waarde ver van 0, 
# dus H0 wordt verworpen, ver in de rechterstaart
# log10(t_tot) = 0.93 + 0.0015 age - 0.0056 education + 0.0070 prod_exp + 0.0057 t_exp 
# - 0.016 t_in + 0.0016  line_in + 0.019 registers_in


#t_in is het minst significant, daarom verwijderen we deze

winkeltijd2 = update(winkeltijd2, .~. - t_in)
summary(winkeltijd2)

#line_in is het minst significant, daarom verwijderen we deze

winkeltijd2 = update(winkeltijd2, .~. - line_in)
summary(winkeltijd2)

#education is het minst significant, daarom verwijderen we deze

winkeltijd2 = update(winkeltijd2, .~. - education)
summary(winkeltijd2)

#age is het minst significant, daarom verwijderen we deze

winkeltijd2 = update(winkeltijd2, .~. - age)
summary(winkeltijd2)

# log10(t_tot) = 0.91 + 0.0064 prod_exp + 0.0063 t_exp + 0.026 registers_in

#De overige variabelen zijn significant genoeg (p-value kleiner dan 0.01)
# Het logaritmisch model is zeer significant
# en heeft iets hogere determinatiecoëfficiënt
# dit is bemoedigend maar motiveert de logaritmische transformatie op zich niet



##We gaan nu voor beide modellen de modelveronderstellingen na
#lineaire model
winkeltijd1res = winkeltijd1$residuals
y_winkel = winkeltijd1$fitted.values
qqnorm(winkeltijd1res)
qqline(winkeltijd1res,col='red')
shapiro.test(winkeltijd1res)

#globale residuplot
plot(y_winkel,winkeltijd1res, xlab = "fitted values", ylab = "residuen", main = "Globale residuplot lineair model")
abline(h=0,col='red')

y_i = winkeltijd1$model[,2] #prod_exp
z_i = winkeltijd1$model[,3] #t_exp
w_i = winkeltijd1$model[,4] #registers_in

hist(prod_exp)
shapiro.test(log(prod_exp))



#individuele residuplots
plot(y_i, winkeltijd1res, xlab = "Verwacht aantal producten", ylab = "residuen", main = "Residuplot prod_exp") 
abline(h=0, col = "red")
plot(z_i, winkeltijd1res) 
abline(h=0)
plot(w_i, winkeltijd1res) 
abline(h=0)

#prod_exp transformeren, wegens slechte residuplot
#logaritmische transformatie van prod_exp
winkeltijd1 = update(winkeltijd1, .~. - prod_exp + log10(prod_exp))
summary(winkeltijd1)

#effect op residuplot: verbetering
plot(y_i, winkeltijd1res, xlab = "Log10(verwacht aantal producten)", ylab = "residuen", main = "Nieuwe residuplot prod_exp") 
abline(h=0, col = "red")

#lognormale model
winkeltijd2res = winkeltijd2$residuals
y_winkel = winkeltijd2$fitted.values
qqnorm(winkeltijd2res)
qqline(winkeltijd2res,col='red')
shapiro.test(winkeltijd2res)

#globale residuplot
plot(y_winkel,winkeltijd2res, xlab = "fitted values", ylab = "residuen", main = "Globale residuplot logaritmisch model")
abline(h=0,col='red')

y_i = winkeltijd2$model[,2] #prod_exp
z_i = winkeltijd2$model[,3] #t_exp
w_i = winkeltijd2$model[,4] #registers_in



#individuele residuplots
plot(y_i, winkeltijd2res, xlab = "verwacht aantal producten", ylab = "residuen", main = "Residuplot prod_exp") 
abline(h=0, col = "red")
plot(z_i, winkeltijd2res) 
abline(h=0)
plot(w_i, winkeltijd2res) 
abline(h=0)

hist(prod_exp)
shapiro.test((prod_exp))
shapiro.test(log10(prod_exp))

#Op de residuplot liggen de punten zeer links. Daarom proberen we een log10 transformatie
winkeltijd2 = update(winkeltijd2, .~. -prod_exp + log10(prod_exp))
summary(winkeltijd2)

y_i = winkeltijd2$model[,2] #prod_exp
plot(y_i, winkeltijd2res, xlab = "log10(verwacht aantal producten)", ylab = "residuen", main = "Nieuwe residuplot prod_exp") 
abline(h=0, col = "red")
#De residuplot is verbeterd

##Onderzoek naar de outliers
resstand = rstandard(winkeltijd1)
plot(sort(abs(resstand)), xlab = "gesorteerde x-waarden", ylab = "Absolute waarde gestandaardiseerde residuen", main = "Outlieronderzoek lineair model")
abline(h=3, col = "tan2", lwd = 2)
which(resstand > 3) #32, 41, 165 uit de oorspronkelijke dataset zijn groter dan 3

winkeltijd3 = lm(t_tot[-c(32,41,165)] ~ t_exp[-c(32,41,165)]  + 
                   registers_in[-c(32,41,165)] + log10(prod_exp)[-c(32,41,165)])
summary(winkeltijd3)
summary(winkeltijd1)

#Het weghalen van deze outliers heeft wel degelijk een significant positief effect op
#het model

resstand = rstandard(winkeltijd2)
plot(sort(abs(resstand)), xlab = "gesorteerde x-waarden", ylab = "Absolute waarde gestandaardiseerde residuen", main = "Outlieronderzoek logaritmisch model")
which(resstand > 3) #165 uit de oorspronkelijke dataset is groter dan 3
which (resstand < -3)

winkeltijd4 = lm(log10(t_tot[-c(165)]) ~ t_exp[-c(165)]  + 
                   registers_in[-c(165)] + log10(prod_exp)[-c(165)])
summary(winkeltijd4)
summary(winkeltijd2)

#Het weghalen van deze outlier heeft geen significant effect en lijkt 
#bij dit model onnodig.

###Dummy variabele week/weekend toevoegen
# 0 = week
# 1 = weekend

W = t_week == "weekend"

winkeltijd5 = lm(t_tot ~ W*t_exp  + 
                   W*registers_in + W*log10(prod_exp))
#Terug reverse regression hierop toepassen
winkeltijd5 = lm(t_tot ~ W*t_exp  + 
                   registers_in + W*log10(prod_exp))

summary(winkeltijd5)

winkeltijd5 = lm(t_tot ~ W*t_exp  + 
                   registers_in + log10(prod_exp))

summary(winkeltijd5)

winkeltijd5 = lm(t_tot ~ W + t_exp  + 
                   registers_in + log10(prod_exp))

summary(winkeltijd5)

winkeltijd5 = lm(t_tot ~ W + t_exp  + 
                   log10(prod_exp))

summary(winkeltijd5)

#Het model is niet significant beter dan winkeltijd 1, daarom lijkt het toevoegen 
#Van deze dummy-variabele niet nuttig bij winkeltijd 1.

#Nu controleren we hetzelfde voor het logaritmische model

winkeltijd6 = lm(log10(t_tot) ~ W*t_exp  + 
                   W*registers_in + W*log10(prod_exp))
summary(winkeltijd6)

winkeltijd6 = lm(log10(t_tot) ~ W*t_exp  + 
                   registers_in + W*log10(prod_exp))
summary(winkeltijd6)

winkeltijd6 = lm(log10(t_tot) ~ W*t_exp  + 
                   registers_in + log10(prod_exp))
summary(winkeltijd6)

winkeltijd6 = lm(log10(t_tot) ~ W + t_exp  + 
                   registers_in + log10(prod_exp))
summary(winkeltijd6)

# Ook hier is er geen verbetering tov model 2, 
# dus is het niet interessant een dummy variabele toe te voegen.

