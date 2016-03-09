#Application 1.7 de valeurs extrêmes

#♣ === Exericice 1 ===

# Q2
n=600
x=rexp(n)


# Q3
k=20
M=rep(NA,n/k)

for(i in 1:(n/k)){
  M[i]=max(x[(1+(i-1)*k):(i*k)])
}

b_n=rep(NA,n/k)
for(i in 1:(n/k)){
  b_n[i]=log(i)
}
M_renormalise=M-b_n

hist(M_renormalise,freq=FALSE)

# Q4
install.packages("evir")
library(evir)
curve(dgev(x,xi=0),add=TRUE,col=2) #marche pas
curve(exp(-x)*exp(-exp(-x)),col=4,add=TRUE)


# Q5
gumbelisation=function(taille_ech, taille_bloc){
  # on suppose que le package "evir" est déjà installé
  n=taille_ech
  k=taille_bloc
  x=rexp(n)
  M=rep(NA,n/k)
  for(i in 1:(n/k)){
    M[i]=max(x[(1+(i-1)*k):(i*k)])
  }
  b_n=rep(NA,n/k)
  for(i in 1:(n/k)){
    b_n[i]=log(i)
  }
  ##M_renormalise=M-b_n
  M_renormalise=M-log(n/k)
  hist(M_renormalise,freq=FALSE)
  curve(exp(-x)*exp(-exp(-x)),col=4,add=TRUE)
  
}

# execrice 2
inv_frechet=function(y){
  # on suppose que y in [0,1]
  return(1/(1-y))
}
gener_frechet=function(n){
  U=runif(n)
  X=inv_frechet(U)
  
}



frechetisation=function(taille_ech, taille_bloc){
  # on suppose que le package "evir" est déjà installé
  n=taille_ech
  k=taille_bloc
  x=gener_frechet(n)
  M=rep(NA,n/k)
  for(i in 1:(n/k)){
    M[i]=max(x[(1+(i-1)*k):(i*k)])
  }
  b_n=rep(NA,n/k)
  for(i in 1:(n/k)){
    b_n[i]=log(i)
  }
  ##M_renormalise=M-b_n
  ##M_renormalise=M*(1/(n/k))
  M_renormalise=M*(n/k)
  hist(M_renormalise,freq=FALSE)
  curve(1/(x*x),col=4,add=TRUE)
  
}
