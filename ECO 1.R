e<-rnorm(100)
x<-runif(100,-5,5)
y<-4+3*x+e 
hist(e)
hist(x)
plot(x,y)
# grafico de dispersion

dx<-x-mean(x)
dy<-y-mean(y)
dxdy<-dx*dy
dx2<-dx^2
b_1<-sum(dxdy)/sum(dx2)
b_0<-mean(y)-b_1*mean(x)

plot(x,y)
abline(a=b_0,b=b_1)

# regresion lineal multiple 

e<-rnorm(100)
x1<-runif(100,-4,4)
x2<-runif(100,0,3)
x3<-runif(100,-10,10)

y<-2+x1+3*x2+4*x3+e

ones=rep(1,100)
X<-cbind(ones,x1,x2,x3)

t(X)

t(X)%*%X

solve(t(X)%*%X)

t(X)%*%y

betas=solve(t(X)%*%X)%*%t(X)%*%y
print(betas)

# utilizando R

reg=lm(y~x1+x2+x3)
summary(reg)

library(wooldridge)
View(wage1)

# utilizando base wooldridge

reg<-lm(wage~educ+exper+tenure+female,data=wage1)
summary(reg)

# ejercicio 

View(alcohol)

reg<-lm(abuse~age+educ+famsize,data=alcohol)
summary(reg)




