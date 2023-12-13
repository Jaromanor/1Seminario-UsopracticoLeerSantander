library(dplyr)
library(tidyr)
library(ggplot2)

SALARIOS <- data.frame(NOMBRE = c("CARLOS", "LUISA", "ADRIANA"), 
                     S_2014 = c(100, 400, 200), 
                     S_2015 = c(500, 600, 700),
                     S_2016 = c(200, 250, 900) )


LARGO<-gather(SALARIOS,AÑO,SALARIO,-1)
ANCHO<-spread(LARGO,AÑO,SALARIO)


LARGO_4<-substring(LARGO$AÑO,3,6)


LARGO_1<-separate(LARGO,AÑO,c("sigla","año"),sep = "_")
LARGO_2<-LARGO_1[,-2]

LARGO_2$NOMBRE<-as.factor(LARGO_2$NOMBRE)
ggplot(LARGO_2,aes(x=año,y=SALARIO))+
  geom_point()

plot(LARGO_2$año,LARGO_2$SALARIO)

ggplot(LARGO_2,aes(x=año,y=SALARIO))+
  geom_point()

LARGO_3<-unite(LARGO_1,AÑO,c(2,3),sep = "*")


# DPLYR

##Seleccionar     select()
##Filtrar         filter()
##Reordenar       arrange()
##Renombrar       rename()
##Crear           mutate()
##Resumir         summarise()
##Agrupar         group_by()

library(gapminder)
df <- gapminder

summary(df)
str(df)

base1<-select(df,c(1,3,4,5,6))
base2<-select(df,-2)
base<-select(df,c(3:6,1))

base3<-filter(df,country=="Spain"|continent=="Asia")

aa <- arrange(df,lifeExp)
ab <- arrange(df,desc(lifeExp))

tr<-rename(df,LE = lifeExp)

ad<-mutate(df,mult = pop*gdpPercap)

ae<-summarise(df,media = mean(lifeExp))

#Media población por país
af<-group_by(df,country)
af<-summarise(af,mean(pop))



#Observaciones por continente
ag<-group_by(df,continent)
ag<-summarise(ag,OBS=n())
ggplot(ag,aes(x=continent,y=OBS))+
  geom_bar(stat = "identity")
#Cuántos países por continente
ah<-group_by(df,continent)
ah<-summarise(ah,Npaises=n_distinct(country))
ggplot(ah,aes(x=continent,y=Npaises,fill=continent))+
  geom_bar(stat = "identity")






df1 <- data.frame(
  municipios = c("Bucaramanga ", "Bucaramanga","Girón", "Floridablanca", "Giron", "Barrancabermeja")
)
categorias_unicas <- unique(df1$municipios)
num_categorias <- length(categorias_unicas)
df1$municipios <- gsub("Giron", "Girón", df1$municipios, fixed = TRUE)

?gsub


  
  




