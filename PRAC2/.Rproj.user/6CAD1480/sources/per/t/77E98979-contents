library(dplyr)
library(writexl)


b1 <- as_tibble(Gobernacion2022potencialesactivos)
b2 <- as_tibble(docregistradosnasenuestra)
b3 <- as_tibble(X5Corte_23_5_22)

a1 <- b2$DOC

b4 <- filter(b1,DOC %in% a1)
str(b4)

a2<-b4$ID

b5 <- filter(b4,DOC %in% a2)
str(b5)

write_xlsx(b5,"activosreales2022.xlsx")
write_xlsx(b4,"registradosreales2022.xlsx")
