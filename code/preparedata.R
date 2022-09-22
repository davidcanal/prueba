#aqu? preparamos los datos


ficed<-read.table(file="data/data-raw/minificed_curso.txt", header=T,sep="\t",na.string="NA",dec=".")
str(ficed)

minificed<-subset(ficed, select=c("NP", "TP", "NPUPAS", "PUPASCUAL","EDREALH","EDREALM", "Year"))
str(minificed)

pairs(minificed)
