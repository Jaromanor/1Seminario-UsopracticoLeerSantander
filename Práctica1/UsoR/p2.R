library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

getwd()

c1<-read_xlsx("E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
              sheet = 1)
c2<-read_xlsx("E:/Académico/1Especialización/RRR/1UsopracticoLeerSantander/Práctica1/UsoR/EstGiron2022.xlsx",
             sheet = 23)

c1<-as_tibble(c1)
c2<-as_tibble(c2)

str(c1)
summary(c1)
head(c1)

c1$FECHA_NACIMIENTO<-as.Date(c1$FECHA_NACIMIENTO)
c1<- c1%>%
  select(-7)%>%
  mutate(gr=substr(c1$GRADO,start = 1,stop = 1))%>%
  mutate_at(vars(1,6,9,10),as.factor)%>%
  mutate(edadd=floor(time_length(ymd(Sys.Date())-ymd(c1$FECHA_NACIMIENTO),
                                 unit = "year")))%>%
  unite(name11,(2:5),sep = " ")












