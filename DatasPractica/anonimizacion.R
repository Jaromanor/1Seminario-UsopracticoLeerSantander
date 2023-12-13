library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

abri2021<-read_xlsx("1.Estudiantes Abril 03 de 2021_Estudiantes.xlsx")
feb2022<-read_xlsx("2.Estudiantes_25feb2022.xlsx")
jul2022<-read_xlsx("3.Estudiantes_7jul2022.xlsx")
usabilidad<-read_xlsx("4.Usabilidad.xlsx")

# ANONIMIZAR

#Vamos a sumarle 5 al número de documento, pero los que tengan letra se les debe
#quitar la letra. Después vamos a dejar un nombre igual a todos

##Anonimizar

###Quitar letras a número de documento y sumar 2


abri2021<-mutate(abri2021,DOC_1=gsub("[^0-9]", "", DOC))
summary(abri2021)
abri2021$DOC_1<-as.numeric(abri2021$DOC_1)
abri2021<-mutate(abri2021,DOC_1=DOC_1+2)


feb2022<-mutate(feb2022,DOC_1=gsub("[^0-9]", "", DOC))
summary(feb2022)
feb2022$DOC_1<-as.numeric(feb2022$DOC_1)
feb2022<-mutate(feb2022,DOC_1=DOC_1+2)

jul2022<-mutate(jul2022,DOC_1=gsub("[^0-9]", "", DOC))
summary(jul2022)
jul2022$DOC_1<-as.numeric(jul2022$DOC_1)
jul2022<-mutate(jul2022,DOC_1=DOC_1+2)

usabilidad<-mutate(usabilidad,ID_1=gsub("[^0-9]", "", ID))
summary(usabilidad)
usabilidad$ID_1<-as.numeric(usabilidad$ID_1)
usabilidad<-mutate(usabilidad,ID_1=ID_1+2)

###Cambiar apellidos y nombres por uno solo

abri2021$APELLIDO1<-"CASAS"
abri2021$APELLIDO2<-"ROJAS"

feb2022$APELLIDO1<-"CASAS"
feb2022$APELLIDO2<-"ROJAS"

jul2022$APELLIDO1<-"CASAS"
jul2022$APELLIDO2<-"ROJAS"

###Eliminamos el número de documento antiguo

abri2021<-abri2021[,-15]
feb2022<-feb2022[,-13]
jul2022<-jul2022[,-13]
usabilidad<-usabilidad[,-7]

###Generación de datas

write_xlsx(abri2021,"Baseabri2021.xlsx")
write_xlsx(feb2022,"Basefeb2022.xlsx")
write_xlsx(jul2022,"Basejul2022.xlsx")
write_xlsx(usabilidad,"Baseusabilidad.xlsx")
