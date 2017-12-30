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

# Spodnja vrstica je lahko zakomentirana ali ne. Glej vrstice 70-73!
#kraji$populacija[which(kraji$kraj =="Ljubljana")] <- 0

###

# https://sl.wikipedia.org/wiki/Volilne_enote_v_Sloveniji
# Slovenija je razdeljena na 8 volilnih enot in 2 posebni za volitve poslancev 
# narodnih skupnosti, ki imajo po cca. 200.000 volilnih upravičencev (oz. 2500 za 
# italijansko in 6500 za madžarsko narodno skupnost). Teh dveh posebnih enot v
# nasem programu ne bomo upostevali. Tako imamo 8 volilnih enot s sedezi v mestih:
# "Kranj", "Postojna", "Ljubljana Center", "Ljubljana Bezigrad", "Celje", "Novo
# mesto", "Maribor" in "Ptuj". Ker pa smo v nasi nalogi upostevali vse prebivalce
# in ne le volilnih upravicencev, bomo dobili nekoliko vecjo stevilko na enoto.
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
sredisca <- c("Kranj", "Postojna", "Ljubljana", "Celje","Novo mesto", "Maribor", "Ptuj")
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
zeljene.velikosti <- rep(293341, length(sredisca)) # za tocne omejitve !!!vrstica 19 zakomentirana!!!
#zeljene.velikosti <- rep(300000, length(sredisca)) # za vecje omejitve !!!vrstica 19 zakomentirana!!!
#zeljene.velikosti <- rep(260000, length(sredisca)) # za brez ljubljane !!!vrstica 19 odkomentirana!!!


# utezi
utezi <- kraji$populacija

# norme oblike (|x1-x2|^p + |y1-y2|^p)^(1/p)
p=2
norme <- function(tocka1, tocka2){
  norma <- ((tocka1[1] - tocka2[1])^p + (tocka1[2] - tocka2[2])^p)^(1/p)
  return(norma)
}

# Uporabili bomo malenkost spremenjen linearni program, kjer je pri nekaterih
# pogojih enacaj zamenjan z "<=".
## Ta korak traja priblizno 1min
source("eksperiment_volitve/LinearniProgram_volitve.R")
resitve <- Linearni.program(tocke, centri, zeljene.velikosti, utezi, norme)


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

ggmap(zemljevid_enot)+geom_point(data = enote, aes(x = lon, y = lat), size = 1, fill=enote$enota, shape = 21) +
  geom_point(data=centri, aes(x = centri_lon, y = centri_lat), size = 5, fill="gray", shape = 21)

