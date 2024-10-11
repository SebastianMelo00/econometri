# y = b0 + b1 X  + U 

x<- runif(100000,-5,5)
u<- rnorm(100000)
y<- 4 + 2 * x + u 
ones<- rep(1,100000)

X<- cbind(ones, x)
# ESTIMACION CON mco
betas<- solve(t(X)%*%X)%*%t(X)%*% y
# estimar con diferentes tamaños de muestra

# algoritmos  
# 1) tomar una muestra de la poblacion  x , y
# 2) con la muestra ajustar el B estimado 
# 3) guardar b1 estimado en un vetor 
# 4) graicar 
# 5) repetir 1000 veces 
# 6) sintaxis para R : for(i in 1:1000){bloque de codigo 1,2,3}

# graficar los resultados

# algoritmo 2do paso 

  beta<- function (X,y){
  betas<- solve(t(X)%*%X)%*%t(X)%*% y 
  return(betas[2])}
  

  betas1<- c()
  sample(1:100000,10)
for (i in 1:1000) {
# algoritmo 1er paso
#  1,1 paso muestra de 10 elementos que vaya desde 1 a 100.000
   s_values<- sample(1:100000, 1000)
#  2,1 paso toma la muestra de 10 datos de x 
   X_s<- X[s_values,]
#  3,1 paso toma la muestra de 10 datos de y 
   y_s<- y[s_values]
   beta1 <- beta(X_s, y_s)
   betas1 <- c(betas1, beta1)}
















