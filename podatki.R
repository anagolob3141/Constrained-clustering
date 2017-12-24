#library(ggplot2)
#library(lpSolve)
library(lpSolveAPI)
#Legenda:
# točke...X
# utezi...omega
# K...stevilo gnezd
# zeljene.velikosti...K
# vsota.utezi = vsota.K���

xkoordinate <- runif(125, 1.3, 20.3)
ykoordinate <- rnorm(125, mean = 15, sd = 10)
tocke <- data.frame(cbind(xkoordinate, ykoordinate))


centri.y.koordinata <- rep(15,20)
centri.x.koordinata <- seq(3 + (17/20), 20, (17/20))
centri<- data.frame(cbind(centri.x.koordinata, centri.y.koordinata))

#ggplot(podatki, aes(xkoordinate, ykoordinate)) + geom_point()
#ggplot(centri, aes(centri.x.koordinata, centri.y.koordinat)) + geom_point()

utezi <- runif(125, 24.5, 204.3)

K <- 20
vsota.utezi <- sum(utezi)
zeljene.velikosti <- rep(vsota.utezi/K, K)

razdalija <- function(tocka1, tocka2){
  norma <- ((tocka1[1] - tocka2[1])^2 + (tocka1[2] - tocka2[2])^2)^(1/2)
  return(norma)
}

funk <- function(ksi, utezi, tocke, centri){
  k <- nrow(ksi)
  m <- ncol(uteci)
  vsota <- 0
  for(i in 1:k){
   for(j in 1:m){
     vsota <- vsota + ksi[i][j] * utezi[j] * razdalija(tocke[j], center[i])
   }
  } 
  return(vsota)
}

funk_lp <- function(utezi, zeljene.velikosti, n, u){
  vsota1 <- 0
  vsota2 <- 0
  for(j in 1:length(utezi)){
    vsota1 <- vsota1 + utezi[j]*n[j]
  }
  for(i in 1:length(zeljene.velikosti)){
    vsota2 <- vsota2 + zeljene.velikosti[i]*u[i]
  }
  return(vsota1-vsota2)
}

m <- length(utezi)
k <- K

f.obj <- c(utezi,-zeljene.velikosti)
f.con <- matrix(0, nrow = m*k, ncol = m+k)
for(i in 1:k){
  for(j in 1:m){
    f.con[(i-1)*m + j,m+i] <- -1
    f.con[(i-1)*m + j,j] <- 1
  }
}
  
#f.con <- matrix(c(1,-1),nrow = m*k, ncol = 2, byrow = TRUE)
f.dir <- rep("<=", k*m)

f.rhs <- c()
for(i in 1:k){
  for(j in  1:m){
    f.rhs[(i-1)*m + j] <- razdalija(centri[i,],tocke[j,])
  }
}

#linearni_program <- lp("max", f.obj, f.con, f.dir, f.rhs)

lprec <- make.lp(m*k,m+k)

for (i in 1:(m+k)){
  set.column(lprec, i, f.con[,i])
}

set.objfn(lprec,f.obj)
set.constr.type(lprec, f.dir)
set.rhs(lprec, f.rhs)
set.bounds(lprec, lower = rep(0,m+k))
lp.control(lprec,sense='max')



solve(lprec)
resitve <- get.variables(lprec)

write.lp(lprec, "lpfilename.lp", "lp")
seznam.resitev <- list("eta" = resitve[1:length(utezi)], "mu" = res[length(utezi)+1:length(zeljene.velikosti)])
#get.dual.solution(lprec)


#####################################################################

f.obj1 <- c()
for(i in 1:k){
  for(j in 1:m){
    f.obj1[(i-1)*m + j] <- razdalija(centri[i,], tocke[j,])*utezi[j]
  }
}


f.con1 <- matrix(0, nrow = m+k, ncol = m*k)
for(j in 1:m){
  stevec <- seq(j, k*m,m)
  for(i in stevec){
    f.con1[j,i] <- 1
  }
}


#f.con1[m+1,(1:m)]<-utezi
for(i in 1:k){
  f.con1[m+i,(((i-1)*m+1):(i*m))]<-utezi
}

f.rhs1 <- c(rep(1,m),zeljene.velikosti)

f.dir1 <- rep("=",m+k)

lprec1 <- make.lp(m+k,m*k)

for (i in 1:(m*k)){
  set.column(lprec1, i, f.con1[,i])
}

set.objfn(lprec1,f.obj1)
set.constr.type(lprec1, f.dir1)
set.rhs(lprec1, f.rhs1)
set.bounds(lprec1, lower = rep(0,m*k))

write.lp(lprec1, "lpfilename1.lp", "lp")
solve(lprec1)
resitve <- get.variables(lprec1)

mat.resitev <- matrix(resitve, nrow = k, ncol = m, byrow = TRUE)
