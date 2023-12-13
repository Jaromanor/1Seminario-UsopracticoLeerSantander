library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

getwd()

d1 <- read_excel("D:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
                 sheet = 1)


d2 <- read_excel("D:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
                 sheet = 23)

d1 <- as_tibble(d1)
d2 <- as_tibble(d2)

str(d1)
str(d2)
d1$FECHA_NACIMIENTO<-as.Date(d1$FECHA_NACIMIENTO)
d2$FECHA_NACIMIENTO<-as.Date(d2$FECHA_NACIMIENTO)
d1 <- d1%>%
  select(-7)%>%
  mutate(gr=substr(d1$GRADO,start = 1,stop = 1))%>%
  mutate(edad=floor(time_length(ymd(Sys.Date())-ymd(d1$FECHA_NACIMIENTO),
                                unit = "year")))%>%
  unite(name,c(2:5),sep = " ")
  
str(d1)
d1 <- mutate_at(d1,vars(1,3,6,7),as.factor)
summary(d1)

d2 <- d2 %>%
  mutate(gr=substr(d2$GRADO,start = 2,stop = 2))%>%
  mutate(edad=floor(time_length(ymd(Sys.Date())-ymd(d2$FECHA_NACIMIENTO),
                                unit = "year")))%>%
  unite(name,c(2:5),sep = " ")
str(d2)  
d2<-mutate_at(d2,vars(1,3,6,7),as.factor)  

h1<-bind_rows(d1,d2)  

h1 <- h1 %>%
  mutate(provincia=NA,municipio=NA,colegio=NA,
         sede=NA,zona=NA,jornada=NA,
         email=NA,cel=NA,icon=NA)%>%
  select(provincia,municipio,colegio,sede,zona,
         jornada,gr,GRADO,name,GENERO,tipo_doc,
         DOC,email,cel,edad,icon)

str(h1)

h1<-rename(h1,grados=gr,curso=GRADO,nombre=name,
           sexo=GENERO,tipodoc=tipo_doc,documento=DOC)

#Busco la existencia de duplicados por número de documento

h1[duplicated(h1$documento),] #No tiene

#Cambio en la variable sexo: Hombre = 0, Mujer = 1

levels(h1$sexo)[levels(h1$sexo)=="FEMENINO"]<-1
levels(h1$sexo)[levels(h1$sexo)=="MASCULINO"]<-0

#Le agrego provincia y municipio

h1$provincia<-"METROPOLITANA"
h1$municipio<-"GIRÓN"

#sede A para grado 4 y sede B para grado 0
str(h1)
h1$sede[h1$grados=="4"]<-"A"
h1$sede[h1$grados==0]<-"B"
h1$colegio<-"COLEGIO ORIENTE MIRAFLORES"

#sede A, si hombre entonces jornada "TARDE", si mujer jornada "MAÑANA"

h11<-filter(h1,sede=="A")
jor1<-ifelse(h11$sexo==0,"TARDE","MAÑANA")
h12<-filter(h1,sede=="B")
jor2<-ifelse(h12$sexo==1,"TARDE","MAÑANA")

jor3<-c(jor1,jor2)

h1$jornada<-jor3

