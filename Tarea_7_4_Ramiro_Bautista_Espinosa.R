library(MASS) 
library(stargazer) 

#RAMIRO BAUTISTA ESPINOSA# 
#Tarea 7_ejercico 4 ECONOMETRÍA# 
#Parámetros de la muestra aleatora:# 

#4#
E<-c(1,0,2) 
VAR<-matrix(c(0.8,0.4,-0.2,0.4,1,-0.8,-0.2,-0.8,2),ncol = 3, byrow = T) 

#Muestra i.i.d.# 
set.seed(2020)
M<-mvrnorm(400,E,VAR) 
VEC1<-rep(1,400) 
X<-cbind(VEC1,M[,2:3]) 
Y<-M[,1] 


#a) b^ = [(X'X)^-1][X'Y]# 

(b1<-solve(t(X)%*%X)%*%(t(X)%*%Y)) 

#b)# 

Suma1<-matrix(c(rep(0,9)),ncol = 3, byrow = T) 
Suma2<-c(rep(0,3)) 


for (i in 1:400) { 
  
  Suma1<-Suma1 + X[i,1:3]%*%t(X[i,1:3]) 
  Suma2<-Suma2 + M[i,1]%*%X[i,1:3] 
  Mat1<-Suma1/400 
  Mat2<-Suma2/400  
  
} 

b2=(solve(Mat1))%*%t(Mat2) 

#c)  

Suma3<-0 
Suma4<-c(rep(0,2)) 
Suma5<-matrix(c(rep(0,4)),ncol = 2, byrow = T) 
Suma6<-0 


for (i in 1:400) { 
  
  Suma3<-Suma3 + M[i,1] 
  Suma4<-Suma4 + M[i,2:3] 
  My<-Suma3/400 
  Mx<-Suma4/400 
  
} 



for (i in 1:400) { 
  
  Suma5<-Suma5 + (M[i,2:3]-Mx)%*%t(M[i,2:3]-Mx) 
  Suma6<-Suma6 + (M[i,1]-My)%*%(M[i,2:3]-Mx) 
  
  VARX<-Suma5/400 
  COVYX<-Suma6/400 
  
  
} 


b3<-matrix(c(rep(0,3)),ncol = 1, byrow = T) 
b3[1]<-My-COVYX%*%solve(VARX)%*%Mx  
b3[2:3]<-solve(VARX)%*%t(COVYX) 

#V)# 

#c)  
Suma7<-0 

for (i in 1:400) { 
  Suma7<-Suma7 + (M[i,1]-My)^2 
  VARY<-Suma7/400 
  
} 

VARU<-VARY-COVYX%*%solve(VARX)%*%t(COVYX) 
