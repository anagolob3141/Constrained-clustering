library(lpSolveAPI)

# Funkcija za reševanje linearnega programa oblike: min <c, x>
#                                                  p.p A*x = b
# Legenda vhodnih parametrov:
# tocke...vektor točk (dimenzije m), ki jih želimo ločiti v grupe,
# centri...vektor točk (dimenzije k), s pomočjo katerih je definirana norma (center[i] je "središče" gruče C[i]),
# zeljene.velikosti...vektor pozitivnih stevil (dimenzije k), i-ta komponenta pove zeljeno vrednost utezi za grupo C[i],
# utezi...vektor z utezmi na posameznih točkah (dimenzije m),
# norme...vektor norm (dimenzije k), ki definirajo razdalijo za posamezno gručo.

Linearni.program <- function(tocke, centri, zeljene.velikosti, utezi, norme){
  
  k <- length(zeljene.velikosti)
  m <- length(utezi)
  
  # Konstruiramo vektor koeficjentov c minimizirane funkcije: min <c, x>
  f.obj1 <- c()
  for(i in 1:k){
    for(j in 1:m){
      f.obj1[(i-1)*m + j] <- norme(centri[i,], tocke[j,]) * utezi[j]
    }
  }
  
  # POGOJI: A*x = b
  
  # Konstruiramo matriko koeficjentov A: 
  f.con1 <- matrix(0, nrow = m+k, ncol = m*k)
  for(j in 1:m){
    stevec <- seq(j, k*m,m)
    for(i in stevec){
      f.con1[j,i] <- 1
    }
  }
  
  for(i in 1:k){
    f.con1[m+i,(((i-1)*m+1):(i*m))]<-utezi
  }
  
  # Konstruiramo vektor koeficjentov b:
  f.rhs1 <- c(rep(1,m),zeljene.velikosti)
  
  # Pri vseh pogojih so uporabljeni enačaji:
  #f.dir1 <- rep("=",m+k)
  # Imamo navzgor omejene velikosti gruc:
  f.dir1 <- c(rep("=",m),rep("<=",k))
  
  # Sestavljanje in rešitev linearnega programa:
  lprec1 <- make.lp(m+k,m*k)
  
  for (i in 1:(m*k)){
    set.column(lprec1, i, f.con1[,i])
  }
  
  set.objfn(lprec1,f.obj1)
  set.constr.type(lprec1, f.dir1)
  set.rhs(lprec1, f.rhs1)
  #set.type(lprec1, m*k, type = "binary")
  #set.bounds(lprec1, lower = rep(0,m*k))
  
  
  
  #write.lp(lprec1, "volitve.lp", "lp")
  solve(lprec1)
  resitve <- get.variables(lprec1)
  
  mat.resitev <- matrix(resitve, nrow = k, ncol = m, byrow = TRUE)
  return(mat.resitev)
}