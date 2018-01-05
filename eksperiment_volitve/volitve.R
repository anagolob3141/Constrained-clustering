library(readr)
library(ggplot2)
library(ggmap)
library(lpSolveAPI)

kraji <- tabela_krajev_populacij_koordinat <- read_csv("eksperiment_volitve/tabela_krajev_populacij_koordinat.csv", 
                                              col_types = cols(lat = col_number(), 
                                                               lon = col_number()))
# Odstranimo kraje, ki imajo vrednosti NA
brisi <- c()
for (i in 1:length(kraji$lon)){
  if (is.na(kraji$lon[i])){
    brisi <- c(brisi, i)
  }
}
kraji <- kraji[-brisi,]

###

# https://sl.wikipedia.org/wiki/Volilne_enote_v_Sloveniji
# Slovenija je razdeljena na 8 volilnih enot in 2 posebni za volitve poslancev 
# narodnih skupnosti, ki imajo po cca. 200.000 volilnih upravičencev (oz. 2500 za 
# italijansko in 6500 za madžarsko narodno skupnost). Teh dveh posebnih enot v
# nasem programu ne bomo upostevali. Tako imamo 8 volilnih enot s sedezi v mestih:
# "Kranj", "Postojna", "Ljubljana Center", "Ljubljana Bezigrad", "Celje", "Novo
# mesto", "Maribor" in "Ptuj". Ker pa smo v nasi nalogi upostevali vse prebivalce
# in ne le volilnih upravicencev, bomo dobili nekoliko vecje stevilo na enoto.
# Prav tako ne bomo razdelili Ljuljane na vec delov, ker nimamo teh podatkov.
#
# V enem primeru bomo imeli tako le 7 volilnih enot: "Kranj", "Postojna", "Ljubljana",
# "Celje", "Novo mesto", "Maribor" in "Ptuj". Na enoto naj bi pripadalo 294884 ljudi.
# Zaradi malo rezerve bomo dopustili do 300000 ljudi na enoto.
#
# V drugem primeru pa bomo upostevali kraj "Ljubljana" kot eno volilno enoto. Da ne
# bomo njenih prebivalcev upostevali v nadaljevanju programa bomo njeno populacijo
# postavili na 0. Ostale enote pa bodo ostale iste kot prej, le da bomo zmanjsali 
# najvec ljudi na enoto na 260000. Tako bomo dobili 8 volilnih enot, od tega dve 
# s srediscem v Ljubljani.

###

# Sedaj nastavimo ustrezne podatke, ki jih bomo uporabili v linearnem programu.

# centri
sredisca <- c("Kranj", "Postojna", "Ljubljana", "Celje", "Novo mesto", "Maribor", "Ptuj")
centri_lon <- c()
centri_lat <- c()

# Nekateri kraji se veckrat pojavijo (v tem primeru kraj Celje), zato bomo za center
# vzeli tisega z najvec prebivalci
for (i in 1:length(sredisca)){
  k=which(kraji$kraj ==sredisca[i])[1]
  for (j in which(kraji$kraj ==sredisca[i])){
    if (kraji$populacija[j] > kraji$populacija[k]){
      k <- j
    }
  }
  centri_lon <- c(centri_lon, kraji$lon[k])
  centri_lat <- c(centri_lat, kraji$lat[k])
}
centri <- data.frame(centri_lon, centri_lat)

# tocke
tocke_lon <- kraji$lon
tocke_lat <- kraji$lat
tocke <- data.frame(cbind(tocke_lon,tocke_lat))

# zeljene velikosti. Vec moznosti, eno izberi.
zeljene.velikosti <- rep(293341, length(sredisca)) # za tocne omejitve 
#zeljene.velikosti <- rep(300000, length(sredisca)) # za vecje omejitve 

# utezi
utezi <- kraji$populacija

# norme oblike (|x1-x2|^p + |y1-y2|^p)^(1/p)
p=2
norma_p <- function(tocka1, tocka2){
  norma <- rep((abs((tocka1[1] - tocka2[1]))^p + abs((tocka1[2] - tocka2[2])^p))^(1/p),length(sredisca))
  return(norma)
}

# Uporabili bomo malenkost spremenjen linearni program, kjer je pri nekaterih
# pogojih enacaj zamenjan z "<=".
## Ta korak traja priblizno 1min
source("eksperiment_volitve/LinearniProgram_volitve.R")
resitve <- Linearni.program.volitve(tocke, centri, zeljene.velikosti, utezi, norma_p)


# Kopiramo tabelo kraji, da bomo dodali noter se zaporedno stevilko enote. Dodoami ji
# nov stolpec "enota"
enote <- kraji
enote[,"enota"] <- NA

for (i in 1:length(kraji$lon)){
  for (k in 1:length(sredisca)){
    if (abs(resitve[k,i]-1)<(10^(-5))){
      enote$enota[i] <- k
    }
  }
}


zemljevid_enot <- get_map(location = c(lon = mean(enote$lon,na.rm=TRUE), lat = mean(enote$lat,na.rm=TRUE)), zoom = 8,
                      maptype = "roadmap", scale = 2)

lj_euclid <- ggmap(zemljevid_enot)+geom_point(data = enote, aes(x = lon, y = lat), size = 1, fill=enote$enota, shape = 21) +
  geom_point(data=centri, aes(x = centri_lon, y = centri_lat), size = 5, fill="gray", shape = 21)

# 1. center -> crna, 2. center -> rdeca, 3. center -> zelena, 4. center -> modra, 
# 5. center -> azzuro, 6. center -> roza, 7. center -> rumena

######

# Tu je primer za elipsoidno normo (angl. ellipsoidal norm)
# Za ta primer rabimo tudi resitev iz prejsnjega primera za p=2.
# Najprej zaokrozimo resitev linearnega programa navzgor, da dobimo matriko z 
# elementi 0 in 1

for (i in 1:length(kraji$lon)){
  for (k in 1:length(sredisca)){
    resitve[k,i] <- round(resitve[k,i])
  }
}

# Po ustrezni formuli dolocimo sredisca
centri_elipt_lon <- c()
centri_elipt_lat <- c()
for (i in 1:length(sredisca)){
  d <- 0
  s <- 0
  for (j in 1:length(kraji$lon)){
    d <- d + resitve[i,j]*utezi[j]*tocke[j,1]
    s <- s + resitve[i,j]*utezi[j]*tocke[j,2]
  }
  centri_elipt_lon[i] <- d/zeljene.velikosti[i]
  centri_elipt_lat[i] <- s/zeljene.velikosti[i]
}
centri_elipt <- data.frame(centri_elipt_lon, centri_elipt_lat)

# Po ustrezni formuli dolocimo pomozne utezi
elipt_utezi <- c()
for (i in 1:length(sredisca)){
  u <- 0
  for (j in 1:length(kraji$lon)){
    u <- u + resitve[i,j]*utezi[j]
  }
  elipt_utezi[i] <- u
}

# Funkcija za dolocanje matrik V_i
v_mat <- function(resitve, el_utezi, el_centri){
  V <- matrix(0,nrow = 2,ncol = 2)
  for (j in 1:length(kraji$lon)){
    v <- unlist(tocke[j,]-el_centri)
    V <- V+(resitve[j]*utezi[j]/el_utezi)*tcrossprod(v,v)
  }
  return(V)
}

# Matrike V_i
V1 <- v_mat(resitve[1,], elipt_utezi[1], centri_elipt[1,])
V2 <- v_mat(resitve[2,], elipt_utezi[2], centri_elipt[2,])
V3 <- v_mat(resitve[3,], elipt_utezi[3], centri_elipt[3,])
V4 <- v_mat(resitve[4,], elipt_utezi[4], centri_elipt[4,])
V5 <- v_mat(resitve[5,], elipt_utezi[5], centri_elipt[5,])
V6 <- v_mat(resitve[6,], elipt_utezi[6], centri_elipt[6,])
V7 <- v_mat(resitve[7,], elipt_utezi[7], centri_elipt[7,])

# S pomocjo singularnega razcepa dolocimo matrike M_i
M1 <- tcrossprod(tcrossprod(svd(V1)$u, diag((svd(V1)$d)^(-1), nrow = 2)), svd(V1)$v)
M2 <- tcrossprod(tcrossprod(svd(V2)$u, diag((svd(V2)$d)^(-1), nrow = 2)), svd(V2)$v)
M3 <- tcrossprod(tcrossprod(svd(V3)$u, diag((svd(V3)$d)^(-1), nrow = 2)), svd(V3)$v)
M4 <- tcrossprod(tcrossprod(svd(V4)$u, diag((svd(V4)$d)^(-1), nrow = 2)), svd(V4)$v)
M5 <- tcrossprod(tcrossprod(svd(V5)$u, diag((svd(V5)$d)^(-1), nrow = 2)), svd(V5)$v)
M6 <- tcrossprod(tcrossprod(svd(V6)$u, diag((svd(V6)$d)^(-1), nrow = 2)), svd(V6)$v)
M7 <- tcrossprod(tcrossprod(svd(V7)$u, diag((svd(V7)$d)^(-1), nrow = 2)), svd(V7)$v)

# Tu imamo norme oblike ||x||_M = [(x^T)*M*x]^(1/2)
# Za vsak M_i imamo svojo normo, ki jih damo v vektor
m <- function(tocka1, tocka2){
  r <- unlist(tocka1-tocka2)
  norma <- c(((tcrossprod(crossprod(r,M1),r))^(1/2)),((tcrossprod(crossprod(r,M2),r))^(1/2)),
             ((tcrossprod(crossprod(r,M3),r))^(1/2)),((tcrossprod(crossprod(r,M4),r))^(1/2)),
             ((tcrossprod(crossprod(r,M5),r))^(1/2)),((tcrossprod(crossprod(r,M6),r))^(1/2)),
             ((tcrossprod(crossprod(r,M7),r))^(1/2)))
  return(norma)
}


# Uporabimo linearni program
resitve_elipt <- Linearni.program.volitve(tocke, centri_elipt, zeljene.velikosti, utezi, m)

# Kopiramo tabelo kraji, da bomo dodali noter se zaporedno stevilko enote. Dodoami ji
# nov stolpec "enota"
enote_elipt <- kraji
enote_elipt[,"enota"] <- NA

for (i in 1:length(kraji$lon)){
  for (k in 1:length(sredisca)){
    if (abs(resitve_elipt[k,i]-1)<(10^(-5))){
      enote_elipt$enota[i] <- k
    }
  }
}


lj_elpit <- ggmap(zemljevid_enot)+geom_point(data = enote_elipt, aes(x = lon, y = lat), size = 1, fill=enote_elipt$enota, shape = 21) +
  geom_point(data=centri_elipt, aes(x = centri_elipt_lon, y = centri_elipt_lat), size = 5, fill="gray", shape = 21)


### 

#Sedaj pa se primer, kjer Ljubljano stejemo kar kot eno enoto in dodamo se eno sredisce v Ljubljano
kraji_l <- kraji
kraji_l$populacija[which(kraji_l$kraj =="Ljubljana")] <- 0

# centri so isti kot v prejsnjem primeru

# tocke so iste kot v prejsnjem primeru

# zeljene velikosti
zeljene.velikosti <- rep(260000, length(sredisca)) # za brez Ljubljane

# utezi
utezi_l <- kraji_l$populacija

# norme oblike (|x1-x2|^p + |y1-y2|^p)^(1/p)
p=2
norma_p <- function(tocka1, tocka2){
  norma <- rep((abs((tocka1[1] - tocka2[1]))^p + abs((tocka1[2] - tocka2[2])^p))^(1/p),length(sredisca))
  return(norma)
}

# Uporabili bomo malenkost spremenjen linearni program, kjer je pri nekaterih
# pogojih enacaj zamenjan z "<=".
## Ta korak traja priblizno 1min
resitve_l <- Linearni.program.volitve(tocke, centri, zeljene.velikosti, utezi_l, norma_p)


# Kopiramo tabelo kraji_l, da bomo dodali noter se zaporedno stevilko enote. Dodoami ji
# nov stolpec "enota"
enote_l <- kraji_l
enote_l[,"enota"] <- NA

for (i in 1:length(kraji_l$lon)){
  for (k in 1:length(sredisca)){
    if (abs(resitve_l[k,i]-1)<(10^(-5))){
      enote_l$enota[i] <- k
    }
  }
}

brez_lj <- ggmap(zemljevid_enot)+geom_point(data = enote_l, aes(x = lon, y = lat), size = 1, fill=enote_l$enota, shape = 21) +
  geom_point(data=centri, aes(x = centri_lon, y = centri_lat), size = 5, fill="gray", shape = 21)

# 1. center -> crna, 2. center -> rdeca, 3. center -> zelena, 4. center -> modra, 
# 5. center -> azzuro, 6. center -> roza, 7. center -> rumena
