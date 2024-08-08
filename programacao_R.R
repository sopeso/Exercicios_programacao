## Resolucao da lista 6
##  Aluna: Ana Luiza de Almeida Pantuza


#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 1 ----
conta = function(n, M, M0, VAR){
  amo = rnorm(n, M, VAR)
  media = mean(amo)
  sd = sqrt(VAR)
  Z = (media - M0)/(sd/sqrt(n))
  #print(Z)
  if(Z < -1.96)
    #print(0)
    J = 0
  if(Z > 1.96)
    #print(0)
    J = 0
  else
    J = 1
    #print(1)
  return(J)
}
conta(10, 0, 0, 1)

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 2 ----

conta2 = function(n, M, M0, VAR){
  repe = as.array(replicate(1000, conta(n, M, M0, VAR), simplify = array))
  return(repe)
}
conta2(30,0,0,1)

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 3 ----

prop = function(x){
  soma = sum(x)
  proporcao = soma/length(x)
  return(proporcao)
}
k = conta2(10,0,0,1)
soma = sum(k)
proporcao = soma/1000
proporcao

prop(k)

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 4 ----

s = sapply(X = c(10,30,25), FUN = conta2, M = 2, M0 = 0, VAR = 3)
s
length(s)
prop(s)

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 5 ----

m=mapply(conta2, n= c(5,10,20,30,32,50),M=c(rep(2,5)),M0=c(0,1,2,3,4,5),VAR= c(rep(1,4)))
m

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 6 ----

fc6<- function(x){
  y= (x^(2) + x^(1/3))
  return(y)
}

integrate(f= fc6,lower=0,upper=1)

M=10000
x= runif(M,0,1)

mean(x^(2) + x^(1/3)) 

#A precisão é boa para integração?
#valor confiável
valor= integrate(f=fc6, lower=0, upper=1)$value

fcc= function(M){
  x= runif(M,0,1)
  return((4*mean((x^(2) + x^(1/3)))))
}

MS= 10^(1:7)
IS= sapply(X= MS , FUN= fcc)
IS


X11()
plot(log(MS, base= 10),IS, type="b",pch=16,col=2,lwd=2)


#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 7 ----

classe_nova = function(x){
  nova_classe = sort(x)
  nova_classe1 = nova_classe[-c(1,2,3)]
  media_nova = mean(nova_classe1)
  return(media_nova)
}
vetor = c(1,2,3,4,5,6,7,8,9)
vetorc = c(19,394,2,2,4,5,65,8,8)
classe_nova(vetor)
classe_nova(vetorc)
vetorz = c(4,5,6,7,8,9)
mean(vetorz)

#===---===---===---===---===---===---===---===---===---===---===---===---===---

# Questao 8 ----

operacao = function(A,B){
  diagonal = diag(diag(A))%*%diag(diag(B))
  return(diagonal)
}
A = matrix(c(1,2,4,3,1,5),byrow=F,ncol=3,nrow = 3)
A
B = matrix(c(4,6,8,2,1,7),byrow=F,ncol=3,nrow = 3)
B
operacao(A,B)



