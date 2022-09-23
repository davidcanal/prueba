library(performance)
library(knitr)
library(dplyr)
library(see)
library(parameters)
library(lme4)
library(tidyverse)
library(DHARMa)
library("performance")
library(sjPlot)
library(glmmTMB)
#Primero metemos los datos

ficed<-read.table(file="data/data-raw/minificed_curso.txt", header=T,sep="\t",na.string="NA",dec=".")
str(ficed)

#hacemos subset 
minificed<-subset(ficed, select=c("NP", "FP", "TP", "NPUPAS", "PUPASCUAL","EDREALH","anillaH", "Year"))
str(minificed)
summary(minificed)
table(minificed$Year)

#Hacemos un primera visualización general de correlaciones

pairs(minificed)
hist(minificed$NP)
plot(minificed$NP, minificed$NPUPAS)

names(minificed)[names(minificed) == "NPUPAS"] <- "Num_ecto"
names(minificed)[names(minificed) == "NP"] <- "Num_pollos"


#Probamos algunos modelos con sentido biológico
  #1. Gaussian
  mod_gaus <- lmer(Num_pollos ~Num_ecto+
                      (1|Year)+ (1|anillaH),
                      data = minificed)
  check_model(mod_gaus) 
  
  
  #2. Poisson
  mod_poi <- glmer(Num_pollos ~Num_ecto+
                     (1|Year)+ (1|anillaH),
                   data = minificed, family ="poisson")
  

  #checked the assumptions of the model
  check_model(mod_poi)
  simulationOutput<- simulateResiduals(fittedModel = mod_poi, n = 999)
  plot(simulationOutput)
  
  
   
  #3 Zero inflated
  mod_poi_zero<- glmmTMB(Num_pollos ~Num_ecto+
                           (1|Year)+ (1|anillaH),
                         data = minificed,zi=~1, family="poisson")
  check_model(mod_poi_zero)
  simulationOutput<- simulateResiduals(fittedModel = mod_poi_zero, n = 999)
  plot(simulationOutput)
  
  summary(mod_poi_zero)
  
#Comparamos los modelos anteriores
AIC(mod_gaus,mod_poi,mod_poi_zero)


#Plot
plot_model(mod_poi_zero, type= "pred", terms = c("Num_ecto"), show.data = T)+
  labs(y="Num pollos voalados", x = "Num ectoparasitos")+theme_classic()


# The end #####-





















###


mod_poi <- glmer(cbin(Num_pollos,TP-Num_pollos) ~Num_ecto+
                   (1|Year)+ (1|anillaH),
                 data = minificed, family ="poisson")


m1 <- glm(TP ~ EDREALH, data = minificed)
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

#Probamos algunos modelos con sentido biológico
mod_gaus <- lmer(TP ~FP+
                   (1|Year)+ (1|anillaH),
                 data = minificed)

check_model(mod_gaus)
diagnostics.plot(mod_gaus) 


mod_poi <- glmer(TP ~FP+
                   (1|Year),
                 data = minificed, family ="poisson")
summary(mod_poi)

#checked the assumptions of the model
simulationOutput<- simulateResiduals(fittedModel = mod_poi, n = 999)
plot(simulationOutput)



compare_performance(mod_gaus,mod_poi)
plot(compare_performance(mod_gaus,mod_poi))






