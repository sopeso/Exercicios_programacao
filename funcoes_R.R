## Resolucao da lista 4
##  Aluno(a) 1: Ana Luiza de Almeida Pantuza
##  Matricula 1: 2018111609

##  Aluno(a) 2: Sophia Araujo de Moraes
##  Matricula 2: 2018076200

#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao 1 ----
f1= function(vetor){
  a= sort(vetor, decreasing = FALSE)
  n=length(vetor)
  for (i in 1:length(a)){
    j= a[n]
    return(j)
  }
  
}
vetor<- c(1,2,3,5,10,20,1)
f1(vetor)


# Confirm we got correct answer
max(vetor)


#Verificando tempo de execução
install.packages("microbenchmark")
library(microbenchmark)
microbenchmark(f1(vetor),max(vetor)) #comparação do tempo computacional

#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao 2 ----

f2<-function(){
  i<-1
  while(i<=5){
    j=sample(2:12, size=1, replace=T)
    k= sample(2:12, size=1, replace=T)
    produto= j*k
    cat("Multiplique os dois números dados:" )
    print(c(j,k))
    enter <- as.integer(readline(prompt = " "))
    if (enter == produto){
      i= i+1
    }
  }
}

f2()
#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao  3----

f3=function(n){
  dados <- rnorm(n, mean = 0,sd = 1)
  matrizB= matrix(0,2,n)
  s<-1
  
  for (i in 1:length(dados)){
    if (i <=0.2){
      matrizB[1,]<-1
      matrizB[2,]<-0.2
    }
    if( i> 0.2|i<=0.7){
      matrizB[1,]<-2
      matrizB[2,]<-0.5
    }
    else{
      matrizB[1,]<-3
      matrizB[2,]<-0.3

    }
  }
  s<-s+1
  return(matrizB)
}
f3(3) # o loop não está caminhando, não sai da primeira posição

#b===
f4=function(n){
  y= sample(1:3, n, replace=T, prob = c( 0.2,0.5, 0.3))
  return(y)
}
f4(5)

#c===
f5=function(n){
  y= sample(1:3, n, replace=T, prob = c( 0.2,0.5, 0.3))
  return(y)
}
bd5=f5(100)

x <- table(bd5) #tabela frequência absoluta

x11()
barplot(x,xlab = "Y",
        ylab = "Frequência absoluta",
        col = c("seagreen", "yellowgreen","green4"),ylim=c(0,55))


#d===



#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao  4----

f6= function(n, lambda){
  X <- rnorm(n, mean = 0,sd = 1)
  y<-(- log(X))/lambda
  y= as.vector(y)
  return(y)
}

# b)----
probabilidadeExp = function(n,lambida,prob){
  
  amostraexp = geraExponencial(n, lambida)
  contadorprob = 0
  for (i in 1:n) {
    if (amostraexp[i] > prob) {
      contadorprob = contadorprob + 1
    }
  }
  
  prob = contadorprob/n
  
  return(prob)
  
  
}

testeprob = probabilidadeExp(10000, 5, 0.5) 
testeprob

# c)----
funcExpo = function(lambida){
  lambida * exp(-1*(lambida)*0.5)
  
}



intnumerica = function(lambida){
  fy = integrate(funcExpo, lower = 0.5, upper = 1)
  return (fy)
  
}


intnumerica(lambida)




#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao  5----

f10<- function(n,mean,sigma){
  lista<- numeric(1000)
  z<- numeric(1000)
  i<-1
  repeat{
    output<-rnorm(n,mean=mean,sd=sigma)
    lista[i]<- round(mean(output),dig=3) #media amostral
    z[i] = round(((lista[i] - mean)/(sigma/sqrt(n))),dig=3)
    i<-i+1
    if(i>1000) break()
  }
  return(z)
}
t= f10(5,0,1)
matrix(t)
t= matrix(t) 

ff = function(t){
  
  contador= 0
  n= length(t)
  for (i in n) {
    if (t[i] >=1.96 | y[i]<=-1.96) {
      contador = contador + 1
    }
  }
  
  prob = contador/n
  
  return(prob)
  
  
}

ff(t)

#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao  6----
amostra<- rnorm(100,0,1)
f12<- function(amostra){
  for ( i in 1: length(amostra)){
    if (i<(-1)) cat<- '0'
    ifelse (i>=-1 | i<=1) cat<- '1'
    else cat<-'2'
    return(amostra)
  }
}


#===---===---===---===---===---===---===---===---===---===---===---===---===-
# Questao  7----

media3<-function(x,type){
  medida<-switch(type,
    n<-length(x),
    mediaaritmetica<-sum(x)/n, 
    mediana<- median(x),
    mediageometrica<-(prod(x))^(1/n),#geometrica
    mediaharmonica<-(1/n*sum(1/x))^(-1))#harmonica
}
