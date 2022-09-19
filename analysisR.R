library(tidyverse)
datos<-read_csv("github_data.csv") 
#dataset: indice de horas trabajadas con felicidad
glimpse(datos)
#hago click en git ->commit. Luego en cada archivo:
#commit : copia local
#push: sincronizo con la nube

plot (happiness ~work.hours, data=datos )
