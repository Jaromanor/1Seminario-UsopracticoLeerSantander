library(dplyr)
library(ggplot2)
View(bdd1)

bdd2 <- bdd1[,-1]
summary(bdd2)
dim(bdd2)

bdd3 <- lapply(bdd2[c(1:7)], as.factor)
bdd3 <- data.frame(bdd3)
head(bdd3)
dim(bdd3)

bdd4 <- bdd2[c(8:50)]
final <- data.frame(bdd3,bdd4)
head(final)
summary(final)

prueba1 <- as_tibble(final)

#Quiero crear una tabla de frecuencias donde se muestre el porcentaje
#por sexo en cada colegio

frecuencias <- prueba1 %>% 
  select(SEXO,COLEGIO) %>% 
  group_by(COLEGIO) %>%
  count(SEXO,COLEGIO) %>%
  mutate(prop=paste(round(prop.table(n)*100,2),"%",sep=""))

#Quiero una cantidad de encuestados por escuela

frecesc <- prueba1 %>%
  select(COLEGIO) %>%
  count(COLEGIO) %>%
  mutate(porc=paste(round(prop.table(n)*100,2),"%",sep=""))


#Quiero estado civil por colegio

freestcivcol <- prueba1 %>%
  select(ESTADO.CIVIL,COLEGIO) %>%
  group_by(COLEGIO) %>%
  count(ESTADO.CIVIL,COLEGIO) %>%
  mutate(porc=paste(round(prop.table(n)*100,2),"%",sep = ""))


#Quiero colegio por estado civil

frecolestciv <- prueba1 %>%
  select(ESTADO.CIVIL,COLEGIO) %>%
  group_by(ESTADO.CIVIL) %>%
  count(ESTADO.CIVIL,COLEGIO) %>%
  mutate(porc=paste(round(prop.table(n)*100,2),"%",sep = ""))

#Vamos a cambiar el nombre de las columnas de frecolestciv

rename(frecolestciv,CANTIDAD=n,PORCENTAJE=porc)


#carguemos mtcars

head(mtcars)

#podríamos querer saber cual es el valor medio de mpg y hp 
#para los vehículos acorde a su cantidad de cilindros. 
#Entonces agrupamos los vehículos por la variable cyl 
#y tomamos la media de mpg y hp.

media <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean(mpg),mean(hp))


# Por ejemplo, si queremos tomar el promedio de mpg y hp 
#para vehículos de 6 y 8 cilindros 
#y peso (wt) superior a 3K lbs, 
#con lo que vimos hasta acá deberíamos hacer algo así:

media1 <- mtcars %>%
  filter(cyl>=6,wt>3) %>%
  group_by(cyl) %>%
  summarise(mean(mpg),mean(hp))






