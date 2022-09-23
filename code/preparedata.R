library(performance)
library(knitr)
library(dplyr)
library(see)
library(parameters)

#Primero metemos los datos

ficed<-read.table(file="data/data-raw/minificed_curso.txt", header=T,sep="\t",na.string="NA",dec=".")
str(ficed)

minificed<-subset(ficed, select=c("NP", "TP", "NPUPAS", "PUPASCUAL","EDREALH","anillaH", "Year"))

str(minificed)


#Hacemos un primera visualización general de correlaciones

pairs(minificed)
hist(minificed$NP)
gg
#Probamos algunos modelos con sentido biológico

m1 <- lm(TP ~ EDREALH, data = minificed)
summary(m1)
plot(m1)
check_model(m1)

m2 <- lm(TP ~ EDREALM, data = minificed)
summary(m2)
plot(m2)
check_model(m2)

m3 <- lm(TP ~ Year, data = minificed)
summary(m3)
plot(m3)
check_model(m3)

#Comparamos los modelos anteriores

compare_performance(m1,m2,m3)
plot(compare_performance(m1,m2,m3))