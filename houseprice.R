verilerim=read.csv(file="C:/Users/Ali/Desktop/Eğitim/_Yükseklisans/1.Dönem/R ile Programlama/RFinal/veriler.csv")
verilerim

head(verilerim)

class(verilerim)
class(verilerim$price)
class(verilerim$lotsize)
class(verilerim$bedrooms)
class(verilerim$bathrooms)
class(verilerim$stories)
class(verilerim$garage)
class(verilerim$driveway)
class(verilerim$recreation)
class(verilerim$fullbase)
class(verilerim$aircon)


mean(verilerim$price)
mean(verilerim$lotsize)
mean(verilerim$bedrooms)
mean(verilerim$bathrooms)
mean(verilerim$stories)
mean(verilerim$garage)

median(verilerim$price)
median(verilerim$lotsize)
median(verilerim$bedrooms)
median(verilerim$bathrooms)
median(verilerim$stories)
median(verilerim$garage)

sd(verilerim$price)
sd(verilerim$lotsize)
sd(verilerim$bedrooms)
sd(verilerim$bathrooms)
sd(verilerim$stories)
sd(verilerim$garage)

summary(verilerim)


fiyat=log(log(verilerim$price))
fiyat

shapiro.test(fiyat)

boxplot(fiyat)

hist(fiyat,freq = FALSE,main = "Ev Fiyat Da??l?mlar?",xlab = "Ev Fiyatlar?",ylab = "Y?zde",col = c("blue","red"))
     
lines(density(fiyat),col="black",lwd=4)
box(which = "plot")

qqnorm(fiyat)
qqline(fiyat)



buyukluk=log(verilerim$lotsize)
buyukluk

shapiro.test(buyukluk)

boxplot(buyukluk)

hist(buyukluk,freq = FALSE,main = "Ev B?y?kl?k Da??l?mlar?",xlab = "Ev B?y?kl?k",ylab = "Y?zde",col = c("blue","red"))

lines(density(buyukluk),col="black",lwd=4)
box(which = "plot")

qqnorm(buyukluk)
qqline(buyukluk)




shapiro.test(verilerim$bedrooms)

boxplot(verilerim$bedrooms)

hist(verilerim$bedrooms,freq = FALSE,main = "Ev Oda Da??l?mlar?",xlab = "Ev Oda Say?s?",ylab = "Y?zde",col = c("blue","red"))

lines(density(verilerim$bedrooms),col="black",lwd=4)
box(which = "plot")

qqnorm(verilerim$bedrooms)
qqline(verilerim$bedrooms)



shapiro.test(verilerim$bathrooms)

boxplot(verilerim$bathrooms)

hist(verilerim$bathrooms,freq = FALSE,main = "Ev Banyo Da??l?mlar?",xlab = "Ev Banyo Say?s?",ylab = "Y?zde",col = c("blue","red"))

lines(density(verilerim$bathrooms),col="black",lwd=4)
box(which = "plot")

qqnorm(verilerim$bathrooms)
qqline(verilerim$bathrooms)



shapiro.test(verilerim$stories)

boxplot(verilerim$stories)

hist(verilerim$stories,freq = FALSE,main = "Kat Say?s? Da??l?m",xlab = "Bodrum Hari? Kat Say?s?",ylab = "Y?zde",col = c("blue","red"))

lines(density(verilerim$stories),col="black",lwd=4)
box(which = "plot")

qqnorm(verilerim$stories)
qqline(verilerim$stories)



shapiro.test(verilerim$garage)

boxplot(verilerim$garage)

hist(verilerim$garage,freq = FALSE,main = "Garaj Say?s? Da??l?m?",xlab = " Garaj Say?s?",ylab = "Y?zde",col = c("blue","red"))

lines(density(verilerim$garage),col="black",lwd=4)
box(which = "plot")

qqnorm(verilerim$garage)
qqline(verilerim$garage)


pie(table(verilerim$driveway),main = "Garaj Yolu Da??l?m?", xlab = "Garaj Yolu(Var m? Yok mu)"
    ,ylab = "",col = c("red","blue"))

pie(table(verilerim$recreation),main = "Dinlenme odas? da??l?m?", xlab = "Dilenme Odas?(Var m? Yok mu)"
    ,ylab = "",col = c("red","blue"))

barplot(table(verilerim$fullbase),main = "Bodrum Kat? Da??l?m?"
, xlab = "Bodrum Kat?(Var m? Yok mu)",ylab = "Adet",names.arg = c("Evet","Hay?r"),col = c("red","blue"))

barplot(table(verilerim$aircon),main = "Merkezi Klima Da??l?m?"
, xlab = "Merkezi Klima(Var m? Yok mu)",ylab = "Adet",names.arg = c("Evet","Hay?r"),col = c("red","blue"))



plot(fiyat~buyukluk,xlab = "Ev Fiyat?",ylab = "Ev B?y?kl???",col=c("blue","red"))


plot(buyukluk~verilerim$garage,xlab = "Ev Fiyat?",ylab = "Ev B?y?kl???",col=c("blue","red"))



bartlett.test (fiyat ~ verilerim$fullbase)
bartlett.test (buyukluk ~ verilerim$recreation)
bartlett.test (verilerim$bedrooms ~ verilerim$bathrooms)
bartlett.test (buyukluk ~ verilerim$garage)

#t.test (fiyat ~ verilerim$driveway)
#t.test (buyukluk ~ verilerim$fullbase)
#t.test (verilerim$bedrooms ~ verilerim$recreation)
#t.test (verilerim$stories ~ verilerim$fullbase)
t.test(fiyat~buyukluk)



t.test(verilerim$bedrooms, verilerim$bathrooms, paired = TRUE, alternative = "two.sided")
t.test(verilerim$stories, verilerim$bedrooms, paired = TRUE, alternative = "two.sided")
t.test(verilerim$bathrooms, verilerim$garage, paired = TRUE, alternative = "two.sided")
t.test(verilerim$garage, verilerim$bedrooms, paired = TRUE, alternative = "two.sided")


anova1=aov(verilerim$garage ~ verilerim$driveway)
anova1
summary(anova1)

TukeyHSD (anova1, conf.level = 0.90)



anova2=aov(verilerim$stories ~ verilerim$fullbase)
anova2
summary(anova2)

anov2tuk=TukeyHSD (anova2)
anov2tuk



anova3=aov(verilerim$bedrooms ~ verilerim$recreation)
anova3
summary(anova3)

anov3tuk=TukeyHSD (anova3, conf.level = 0.90)
anov3tuk

anova4=aov(verilerim$garage ~ verilerim$driveway)
anova4
summary(anova4)

anov4tuk=TukeyHSD (anova4, conf.level = 0.90)
anov4tuk





regresyon1= lm(fiyat ~ verilerim$recreation)
summary(regresyon1)

plot(regresyon1)


regresyon2= lm(buyukluk ~ verilerim$stories)
summary(regresyon2)

plot(regresyon2)


regresyon3= lm(fiyat ~ buyukluk)
summary(regresyon3)

plot(regresyon3)



regresyon4= lm(fiyat ~ verilerim$garage)
summary(regresyon4)

plot(regresyon4)







