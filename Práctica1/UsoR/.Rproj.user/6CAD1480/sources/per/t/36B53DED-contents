library(readxl)
#Reviso en qué área de trabajo estoy
getwd()
#Creo un objeto con la dirección de trabajo por facilidad
a<-"E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx"
#cargo las bases de datos que se encuentran en 3 hojas del excel
b1<-read_excel(path=a,sheet = 1)
View(b1)
b2<-read_excel(a,sheet = 22)
b3<-read_excel(a,sheet = 23)
#cargo librería dplyr y transformo las datas en marcos dplyr
library(dplyr)
b1 <- tibble(b1)
b2 <- tibble(b2)
b3 <- tibble(b3)
#analizo base b1
str(b1)
##veo que la columna ...7 no tiene valores, por lo tanto debo eliminar
b1<-select(b1,-7)
## Reviso la existencia de valores faltantes
summary(b1)
##Convierto a factores algunos campos
b1 <- mutate_at(b1,vars(1,6,9),as.factor)
##reviso nuevamente
summary(b1)
##Creo una nueva columna llamada gr: ella muestra el nivel académico y 
##la convierto en factor
b1<-mutate(b1,gr=substr(b1$GRADO,start = 1,stop = 1))
b1<-mutate_at(b1,vars("gr"),as.factor)
#Analizo b2
str(b2)
##Busco NAs
summary(b2)
##Creo una nueva columna llamada gr: ella muestra el nivel académico y 
##convierto en factor las columnas necesarias
b2<-mutate(b2,gr=substr(b2$GRADO,start=3,stop = 3))
b2<-mutate_at(b2,vars(1,6,9,10),as.factor)
summary(b2)
#Analizo b3
str(b3)
##Miro si tiene NAs
summary(b3)
##Creo una nueva columna llamada gr: ella muestra el nivel académico y 
##convierto en factor las columnas necesarias
b3 <- b3%>%mutate(gr=substr(b3$GRADO,start = 1,stop = 2))%>%
  mutate_at(vars(1,6,9,10),as.factor)
summary(b3)

#Una vez analizadas vamos a unirlas
##método base
g1<-rbind.data.frame(b1,b2,b3)
##método dplyr
g2 <- bind_rows(b1,b2,b3)
##qué clase son:
class(g1)
class(g2)

#Una vez unidos queremos crear una columna que muestre la edad
#a partir de la fecha de nacimiento
##Instalo el paquete lubridate
library(lubridate)
##busco la columna FECHA_NACIMIENTO y la paso a formato Date
##esto lo hago porque voy a utilizar la fecha del sistema que viene
##en formato Date (unidades iguales)
fechas<-as.Date(g1$FECHA_NACIMIENTO)
##con floor redondeo años hacia abajo
##con time_length me devuelve en años, unit="year"
##ymd crea formato año, mes, día 
edad1<-floor(time_length(ymd(Sys.Date()) - ymd(fechas), 
                  unit = "year"))
g1 <- mutate(g1,EDAD=edad1)

#Ahora uno los apellidos y nombres en una sola columna llamada nombres

library(tidyr)
g3<-unite(g1,nombres,c(2:5),sep = " ")


#Agrego columnas o las quito para que cuadren con lo pedido por soporte técnico

g4 <- mutate(g3,provincia=NA,sede=NA,
             zona=NA,jornada=NA,email=NA,cel=NA,icon=NA,municipio=NA,colegio=NA)

#Ordeno las columnas como las requiere soporte técnico

g5 <- select(g4,c(9,16,17,10:12,7,1,2,6,3,4,13:15,-5))


#renombro los nombres de las columnas

g6<-rename(g5,grados=gr,curso=GRADO,
           nombre=nombres,sexo=GENERO,tipodoc=tipo_doc,documento=DOC)

#Elimino duplicados de la misma data (basados en el número de documento)

## puedo revisar primero si existe algún duplicado por número de documento
## pero primero lo haré como una práctica buscar duplicado por curso

g61<-g6[duplicated(g6$curso),] #Se debe tener en cuenta
#que esto arroja los valores duplicados teniendo como referencia el primero
#que aparece en las filas, es decir, si elimino los duplicados me deja el
#primero de la lista como único y borra los demás

g6[duplicated(g6$documento),] #En este caso ningún documento se repite

#Ahora elimino duplicados de la base de datos que tiene soporte técnico
#con la que la base datos que estoy trabajando

##En este ejemplo solo pondremos código como para saber

#### a<-data$documentos
###g7<-filter(g6,!documento %>% a)


#Cambiar sexo: hombre=0, mujer=1

##g6$sexo[g6$sexo=="FEMENINO"]<-1 no sirve ya que sexo es factor

levels(g6$sexo)[levels(g6$sexo)=="FEMENINO"] <- 1
levels(g6$sexo)[levels(g6$sexo)=="MASCULINO"] <- 0



#Digamos que los de 4 son provincia Yariguíes, 11 y 10 provincia Vélez, pero 11 
#son de Chipatá y 10 de Cimitarra

g6$provincia[g6$grados==4]<-"YARIGUÍES"
g6$provincia[g6$grados==10 | g6$grados==0]<-"VÉLEZ"
g6$municipio[g6$grados==4]<-"GIRÓN"
g6$municipio[g6$grados==0]<-"CHIPATÁ"
g6$municipio[g6$grados==10]<-"CIMITARRA"

#Nombre del colegio

g6$colegio[is.na(g6$colegio)]<-"COLSANDER"

#Asignar que sea zona urbana

g6$zona[is.na(g6$zona)]<-"URBANA"

#Imprimo mi nueva base de datos

write.csv(g6,"prueba1.csv")








##Forma reduciendo líneas
getwd()
b11 <- read_xlsx("E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
                 sheet = 1)

b11 <- as_tibble(b11)
str(b11)
summary(b11)
b11$FECHA_NACIMIENTO<-as.Date(b11$FECHA_NACIMIENTO)
b11 <- b11 %>%
  mutate(gr=substr(b11$GRADO,start = 2,stop = 2))%>%
  mutate(edad2=floor(time_length(ymd(Sys.Date())-ymd(b11$FECHA_NACIMIENTO),
                                 unit="year")))%>%
  mutate_at(vars(1,6,10),as.factor)%>%
  select(-7)%>%
  unite(name1,(2:5),sep = " ")

