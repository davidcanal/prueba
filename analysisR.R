library(tidyverse)
datos<-read_csv("github_data.csv") 
#dataset: indice de horas trabajadas con felicidad
glimpse(datos)
#Pasos para sincronizar con git
#1 Guarda cambios como siempre (icono disco)
#2. Click en git en git ->commit. 
#3. Selecciona los archivos a sincronizar:
#commit : copia local
#push: sincronizo con la nube

plot (happiness ~work.hours, data=datos)

ggplot(datos)+
  geom_point(aes(work.hours, happiness))+theme_classic()

plot()
