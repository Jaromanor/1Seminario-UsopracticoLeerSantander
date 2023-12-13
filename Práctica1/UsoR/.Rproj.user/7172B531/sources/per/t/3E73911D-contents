library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

ls()
getwd()
z1<-read_xlsx("E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
              sheet = 1)
z2<-read_xlsx("E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
              sheet = 23)

str(z1)
str(z2)

z1$FECHA_NACIMIENTO<-as.Date(z1$FECHA_NACIMIENTO)
z1<-z1%>%
  select(-7)%>%
  mutate(curso=substr(z1$GRADO,start = 1,stop = 1))%>%
  mutate(edad=floor(time_length(ymd(Sys.Date())-ymd(z1$FECHA_NACIMIENTO),
                                unit = "year")))%>%
  unite(nombre,(2:5),sep = " " )
View(z1)

z2$FECHA_NACIMIENTO<-z2$FECHA_NACIMIENTO
z2 <- z2%>%
  mutate(curso=substr(z2$GRADO,start = 2,stop = 2))%>%
  mutate(edad=floor(time_length(ymd(Sys.Date())-ymd(z2$FECHA_NACIMIENTO),
                                unit = "year")))%>%
  unite(nombre,(2:5),sep = " ")
View(z2)

z3 <- bind_rows(z1,z2)
View(z3)

z3 <- z3%>%
  mutate_at(vars(3,6,7),as.factor)%>%
  select(7,1,2,6,3,4,8)
summary(z3)

levels(z3$GENERO)[levels(z3$GENERO)=="FEMENINO"]<-1
levels(z3$GENERO)[levels(z3$GENERO)=="MASCULINO"]<-0

z3<-rename(z3,grados=curso,curso=GRADO,sexo=GENERO,
           tipodoc=tipo_doc,documento=DOC)

z3 <- mutate(z3,provincia=NA,municipio=NA,colegio=NA,
             sede=NA,zona=NA,email=NA,cel=NA,icon=NA)


  