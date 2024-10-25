#punto 1
library(wooldridge)
View(wage1)
#nombrar variables

educ<-wage1$educ
salario<-wage1$wage
lnsalario<-wage1$lwage
restax<-(educ-mean(educ))
restay<-(salario-mean(salario))
varx<-(restax)^2
restayLn<-(lnsalario-mean(lnsalario))
MeanyLn<-(mean(lnsalario))

#Hallar parametros
mean(educ)
mean(salario)

#B1
b1<-sum(restax*restay)/sum(varx)
#B0
b0<-(mean(salario)-b1*mean(educ))

#Parametros Ln
#B1Ln
b1Ln<-sum(restax*restayLn)/sum(varx)
#B0Ln
boLn<-(MeanyLn-b1Ln*mean(educ))

#1.2
reg1<-lm(salario~educ)
summary(reg1)
reg2<-lm(lnsalario~educ)
summary(reg2)

#1.4
Trm<-(4311.80)
Conversion<-(b1*Trm)

#Punto 2

#punto2.1
reg2_1<-lm(wage~female,data=wage1)
summary(reg2_1)

#punto2.2
reg2_1<-lm(wage~female,data=wage1)
summary(reg2_1)

reg2_2<-lm(wage~female+educ,data=wage1)
summary(reg2_2)

reg2_3<-lm(wage~female+educ+I(educ*female),data=wage1)
summary(reg2_3)


#Punto 3 







