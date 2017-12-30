# !!! TE DATOTEKE NE POGANJAT. TO SMO NAREDILI LE ZA PRIDOBITEV KOORDINAT,
# KI SO SHRANJENE V DATOTEKO. ZA NADALJNO DELO SMO UPORABLJALI TO DATOTEKO !!!

library(readr)
library(ggmap)

# Uvozimo podatke za stevilo ljudi za Slovenske kraje.
imena_stolpcev <- c("stevilka", "ime", "populacija", "moski", "zenske")
populacije_krajev_koord <- read_delim("uvoz/populacije_krajev.csv", 
                                ";", escape_double = FALSE, 
                                trim_ws = TRUE, skip = 5, col_names = imena_stolpcev)

# To bo vektor cistih imen krajev, ki ga bomo potrebovali na kocnu za lepo urejeno koncno tabelo
imena_krajev <- populacije_krajev_koord$ime

# Kraji, ki so napisani z velikim crkami (oz. ki imajo stevilo v stolpcu
# "stevilka" manjse od 1000) se dvakrat ponovijo, zato jih je treba odstraniti
# Hkrati bomo tudi izbrisali stevilke iz imenov krajev in ker pred imenom nato
# ostane še presledek, bomo izbrisali še prvi znak.
i=1
vrstice_za_izbrisat <- c()
sredisca <- c()

for (i in 1:length(populacije_krajev_koord$stevilka)){
  if (is.na(populacije_krajev_koord$stevilka[i])){
    vrstice_za_izbrisat <- c(vrstice_za_izbrisat,i)
  }
  else if (as.numeric(populacije_krajev_koord$stevilka[i]) < 1000) {
    vrstice_za_izbrisat <- c(vrstice_za_izbrisat,i)
    sredisca <- c(sredisca,gsub('[[:digit:]]+', '', populacije_krajev_koord$ime[i]))
  }
  else if (as.numeric(populacije_krajev_koord$stevilka[i]) > 1000) {
    populacije_krajev_koord$stevilka[i] <- substr(populacije_krajev_koord$stevilka[i], 1, nchar(populacije_krajev_koord$stevilka[i])-3)
  }
  
  # Hkrati ocistimo vekotr "imena_krajev" neporebnih znakov:
  # Izbrisemo stevilke iz imen:
  imena_krajev[i] <- gsub('[[:digit:]]+', '', imena_krajev[i])
  # Izbrisemo prvi znak, torej presledek pred imenom:
  imena_krajev[i] <- substring(imena_krajev[i], 2, nchar(imena_krajev[i]))
  i <- i+1
}

populacije_krajev_koord[, c(1)] <- sapply(populacije_krajev_koord[, c(1)], as.numeric)

# Sedaj izbrisemo ponavljajoce se kraje v tabeli in v vekotrju
populacije_krajev_koord<-populacije_krajev_koord[-vrstice_za_izbrisat,]
imena_krajev <- imena_krajev[-vrstice_za_izbrisat]

# Vsakemu kraju dodamo pripadajoc vecji kraj, zaradi iskanja geografske dolzine in sirine
for (i in 1:length(populacije_krajev_koord$stevilka)){
  if (populacije_krajev_koord$stevilka[i] >= 145){
    populacije_krajev_koord$ime[i] <- paste(populacije_krajev_koord$ime[i], sredisca[populacije_krajev_koord$stevilka[i]-1], sep = "")
    
  }
  else {populacije_krajev_koord$ime[i] <- paste(populacije_krajev_koord$ime[i], sredisca[populacije_krajev_koord$stevilka[i]], sep = "")}
  # Sedaj izbrisemo stevilke iz imen:
  populacije_krajev_koord$ime[i] <- gsub('[[:digit:]]+', '', populacije_krajev_koord$ime[i])
  # Sedaj pa se prvi znak, torej presledek pred imenom:
  populacije_krajev_koord$ime[i] <- substring(populacije_krajev_koord$ime[i], 2, nchar(populacije_krajev_koord$ime[i]))
}

# Zbrisemo presledek se pred sredisci
sredisca <- substring(sredisca,2,nchar(sredisca))

# Naredimo vektor s pripadajocimi vecjimi mesti, ki ga bomo na koncu dodali tabeli. 
# Zakaj, bomo videli pozneje-
obmocja <- c(NA)
for (i in 1:length(populacije_krajev_koord$stevilka)){
  if (populacije_krajev_koord$stevilka[i] >= 145){
    obmocja[i] <- sredisca[populacije_krajev_koord$stevilka[i]-1]
  }
  else {
    obmocja[i] <- sredisca[populacije_krajev_koord$stevilka[i]]
  }
}


# Sedaj lahko izbrisemo stolpec "stevilka" in tudi "moski" in "zenske".
populacije_krajev_koord[c(1,4,5)] <- c(NULL)

#################################################################################

# prekopiramo tabelo v novo tabelo, da delamo s to naprej
koord_1 <- populacije_krajev_koord

namesv <- c("lon", "lat")
koord_1[, namesv] <- NA


# Ta zanka nam doloci koordinate krajev z virom v "Data Science Toolkit". mogoce je ta
# vir malo manj natancen kot Google, vendar Google ima omejitev na 2500 ponovitev funkcije 
# "geocode" na dan, zato bomo najprej naredili s "source = "dsk"", nato pa tiste ki niso
# vredu še s Googlom.
# Pozor: zanka traja zelo dolgo casa.
# Opomba: lahko bi bolj preprosto uporabili funkcijo "geocode" na tabeli, vendar sem zele,
#         da mi izpisuje na katerem koraku smo (vrstica "print(i)").

for (i in c(1:length(koord_1$ime))){
  j=0
  while(is.na(koord_1[i,"lon"])==TRUE){
    j<-j+1
    koord <- geocode(koord_1$ime[i], source = "dsk")
    koord_1[i,"lon"] <- koord[1]
    koord_1[i,"lat"] <- koord[2]
    if (j>2) break # zaustavitveni pogoj - ce kraja ne najde v treh poizkusih, naj ga izpusti
  }
  print(i)
}

# Datoteka z nepresiscenemi koordinatami:
# write.csv(koord_1, "koord_prvi_del")

# Sedaj bomo pogledali, katere koordinate segajo zunaj mej Slovenije. Za meje semo vzeli
# pravokotink, ki ima za stranice na najbolj vzhodni, zahodni, severni in juzni tocki
# Slovenije (morda segajo celo malo cez meje).

so_vredu <- c()
a1 <- 13.384094
b1 <- 16.636047
a2 <- 45.413105
b2 <- 46.896988
for (i in c(1:length(koord_1$ime))){
  if (((koord_1$lon[i]-a1)>0) && (b1-koord_1$lon[i]>0) && ((koord_1$lat[i]-a2)>0) && (b2-koord_1$lat[i]>0) && is.na(koord_1$lon[i])==FALSE){
    so_vredu <- c(so_vredu,koord_1$X1[i])
  }
}

niso_vredu <- c()
for (i in c(1:length(koord_1$ime))){
  if (is.element(i, so_vredu)==FALSE){
    niso_vredu <- c(niso_vredu, i)
  }
}

# Sedaj tiste, ki niso vredu, poiscemo se enkrat (tokrat z googlom).

# Tiste, ki niso vredu, nastavimo spet na "NA"
for (i in niso_vredu){
  koord_1$lon[i] <- NA
  koord_1$lat[i] <- NA
}

# Za tiste, ki niso vredu, ponovimo postopek dolocanja koordinat (z googlom)
for (i in niso_vredu){
  j=0
  while(is.na(koord_1[i,"lon"])==TRUE){
    j<-j+1
    koord <- geocode(koord_1$ime[i])
    koord_1[i,"lon"] <- koord[1]
    koord_1[i,"lat"] <- koord[2]
    if (j>2) break
  }
  print(i)
}

# Sedaj ponovimo poiscemo tiste kraje, ki jih je postavilo zunaj Slovenije.

so_vredu_2 <- c()
a1 <- 13.384094
b1 <- 16.636047
a2 <- 45.413105
b2 <- 46.896988
for (i in c(1:length(koord_1$ime))){
  if (((koord_1$lon[i]-a1)>0) && (b1-koord_1$lon[i]>0) && ((koord_1$lat[i]-a2)>0) && (b2-koord_1$lat[i]>0) && is.na(koord_1$lon[i])==FALSE){
    so_vredu_2 <- c(so_vredu_2,koord_1$X1[i])
  }
}

niso_vredu_2 <- c()
for (i in c(1:length(koord_1$ime))){
  if (is.element(i, so_vredu_2)==FALSE){
    niso_vredu_2 <- c(niso_vredu_2, i)
  }
}

# Ponovno naredimo kopijo tabele
koord_2 <- koord_1


# Tiste, ki niso vredu, ponovno nastavimo spet na "NA"
for (i in niso_vredu_2){
  koord_2$lon[i] <- NA
  koord_2$lat[i] <- NA
}

#write.csv(koord_2,"koord_drugi_del")

# Sedaj imamo tabelo s koordinatami oz. z "NA" tam, kjer ni najdlo ustreznega kraja. Vidimo,
# da se pri nekaterih krajih koordinate ponavljajo. To je zato, ker namesto nekaterih manjsih
# krajev je najdlo bliznji vecji kraj. To pa ne bo pomembneje vplivalo na rezultat nase naloge.

# Sedaj bomo naredili se koncno tabelo, ki bo imela za stolpce "kraj", "obmocje", "populacija",
# "lon" in "lat"
koncna_imena_stolpcev <- c("kraj", "obmocje", "populacija", "lon", "lat")
tabela_koordinat <- koord_2

tabela_koordinat["ime"] <- imena_krajev
tabela_koordinat[5] <- obmocja
tabela_koordinat <- tabela_koordinat[,colnames(tabela_koordinat)[c(1,5,2:4)]]
names(tabela_koordinat) <- koncna_imena_stolpcev

# Pridobljene podatke smo shranili v datoteko "tabela_krajev_populacij_koordinat.csv"
write.csv(tabela_koordinat,"tabela_krajev_populacij_koordinat.csv")