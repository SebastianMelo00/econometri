#transformacion de variables 
#independiente  dependiente  estandarizar

library(wooldridge)
library(modelsummary)
library(dplyr)

#con library dplyr

bwght_alt<-bwght|>mutate(zbw=(bwght-mean(bwght))/sd(bwght),
                         zfi=(faminc-mean(faminc))/sd(faminc),
                         zcigs=(cigs-mean(cigs))/sd(cigs))
View(bwght_alt)

View(bwght)

#corriendo la regresion

reg1<- lm(bwght ~ faminc, data = bwght)
reg2<- lm(bwght ~ cigs, data = bwght)
reg3<- lm(bwght ~ cigs + faminc, data = bwght)
reg4<- lm(bwghtlbs ~ cigs + faminc, data = bwght)
reg5<- lm(bwght ~ packs + faminc, data = bwght)
reg6<- lm(zbw~zfi + zcigs, data = bwght_alt)
#list = diccionario , asociar el modelo a info

modelos<- list("modelo1"= reg1, "modelo2"= reg2, "modelo3"= reg3, "modelo4"= reg4, "modelo5"= reg5, "modelo6"= reg6)

modelsummary::modelsummary(modelos)

#ejercicio 

sigma_cigs<-sd(bwght$cigs)
sigma_bwght<-sd(bwght$bwght)
bjest=-0.463









