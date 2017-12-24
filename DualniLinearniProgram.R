library(lpSolveAPI)

# Funkcija za reševanje linearnega programa oblike: max <c, x>
#                                                  p.p A*x <= b
# Legenda vhodnih parametrov:
# tocke...vektor točk (dimenzije m), ki jih želimo ločiti v grupe,
# centri...vektor točk (dimenzije k), s pomočjo katerih je definirana norma (center[i] je "središče" gruče C[i]),
# zeljene.velikosti...vektor pozitivnih stevil (dimenzije k), i-ta komponenta pove zeljeno vrednost utezi za gruco C[i],
# utezi...vektor z utezmi na posameznih točkah (dimenzije m),
# norme...vektor norm (dimenzije k), ki definirajo razdalijo za posamezno gručo.

Dualni.linearni.program <- function(zeljene.velikosti, tocke, centri, norme, utezi){
  m <- length(utezi)
  k <- length(zeljene.velikosti)
  
  # Konstruiramo vektor koeficjentov c maksimizirane funkcije: max <c, x>
  # Utezi so koeficienti za pomozne spremenljivke "eta", zeljene velikosti pa za spremenljivke "mu"
  f.obj <- c(utezi,-zeljene.velikosti)
  
  # POGOJI: A*x <= b
  
  # Konstruiramo matriko koeficjentov A: 
  f.con <- matrix(0, nrow = m*k, ncol = m+k)
  for(i in 1:k){
    for(j in 1:m){
      f.con[(i-1)*m + j,m+i] <- -1
      f.con[(i-1)*m + j,j] <- 1
    }
  }
  
  # Konstruiramo vektor koeficjentov b:
  f.rhs <- c()
  for(i in 1:k){
    for(j in  1:m){
      f.rhs[(i-1)*m + j] <- norme[i](centri[i,],tocke[j,])
    }
  }
  
  # Pri vseh pogojih so uporabljeni neenačaji <=:
  f.dir <- rep("<=", k*m)
  
  # Sestavljanje in rešitev linearnega programa:
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
  
  # Prvih length(utezi) so pomozne spremenljivke "eta", ostale (length(zeljene.velikosti)) so spremenljivke "mu"
  resitve <- get.variables(lprec)
  
  seznam.resitev <- list("eta" = resitve[1:length(utezi)], "mu" = res[length(utezi)+1:length(zeljene.velikosti)])
  # Ko klices funkcijo, do "eta" spremenljiv dostopas z ukazom "seznam.resitev$eta",
  # do "mu" spremenljiv pa z ukazom "seznam.resitev$mu"
  
  write.lp(lprec, "duallp.lp", "lp")
  return(seznam.resitev)
}
  
