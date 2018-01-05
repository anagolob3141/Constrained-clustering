# Grupiranje stanovanj v Ljubljani razpoložljivih za prodajo,
# glede na njihovo podobnost.

# 1. Uvoz podatkov:########################################################################################
stanovanja.tipi <- read.csv("eksperimentStanovanja\\tipi.csv")
stanovanja.ostalo <- read.csv("eksperimentStanovanja\\stanovanja.csv")

stanovanja.podatki <- merge(stanovanja.tipi,stanovanja.ostalo,by="id")
stanovanja.podatki <- stanovanja.podatki[order(stanovanja.podatki$velikost),]

# 2. Pregled podatkov za lažjo predstavo:##################################################################
ggplot(stanovanja.podatki, aes(x = podrocje)) +
  geom_bar(aes(fill = tip))

graf <- plot_ly(stanovanja.podatki, x= ~velikost, y= ~cena, 
                z= ~podrocje, color = ~podrocje) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Velikost'),
                      yaxis = list(title = 'Cena'),
                      zaxis = list(title = 'Lokacija')))
graf

# 3. Oblikovanje optimizacijskega problema:

tocke2 <- data.frame(stanovanja.podatki)
m <- nrow(stanovanja.podatki) # št. točk
k2 <- 20 # št. gnezd na katere selimo točke
utezi2 <- rep(1, m)

# Gnezda imajo enakomerne utezi, ce jih ni mozno celostevilko 
# razdeliti na k delov, ostanek dobi srednji.
zeljene.utezi2 <- rep((m %/% k2), k2)
zeljene.utezi2[k2 %/% 2] <- zeljene.utezi2[k2 %/% 2] + m %% k2

###########################################################################################################

# Centri glede na ceno:
povprecje.cena <- mean(as.numeric(stanovanja.podatki$cena))
odklon.cena <- sd(as.numeric(stanovanja.podatki$cena))
centri.cena2 <- c(povprecje.cena - odklon.cena, povprecje.cena + odklon.cena) 
# Centri glede na velikost:
povprecje.velikost <- mean(as.numeric(stanovanja.podatki$velikost))
odklon.velikost <- sd(as.numeric(stanovanja.podatki$velikost))
centri.velikost2 <- c(povprecje.velikost - odklon.velikost, povprecje.velikost + odklon.velikost)
# Centri glede na lokacijo:
centri.podrocje2 <- c("Bezigrad","Center", "Moste-Polje","Siska","Vic-Rudnik")

vsi.centri2 <- expand.grid(centri.cena2, centri.velikost2, centri.podrocje2)

colnames(vsi.centri2) <- c("cena.centri", "velikost.centri", "podrocje.centri")
imena.gnezd <- c()
for(i in 1:k2){
  imena.gnezd[i] <- as.character(i)
}
vsi.centri2 <- cbind(vsi.centri2,imena.gnezd)
vsi.centri2 <- vsi.centri2[order(vsi.centri2$velikost.centri),]
vsi.centri2[11:20,1] <- vsi.centri2[11:20,1] + 90000
vsi.centri2 <- vsi.centri2[order(vsi.centri2$cena.centri),]
vsi.centri2[11:20,2] <- vsi.centri2[11:20,2] + 50

###########################################################################################################

# Definirane razdalje:
# S tema vrednostma bomo normirali podatke lažjo definiranje norm:

druga.norma2 <- function(center, tocka,povprecna.cena, povprecna.velikost, vrednost.podrocje){
  norma.cena <- abs(as.numeric(tocka[1,8]) - as.numeric(center[1,1])/povprecna.cena)^2
  norma.velikost <- abs(as.numeric(tocka[1,7]) - as.numeric(center[1,2])/povprecna.velikost)^2
  norma.podrocje <- vrednost.podrocje
  if(identical(as.character(tocka[1,4]),as.character(center[1,3]))){
    norma.podrocje <- 0
  }
  norma <- (norma.cena + norma.velikost + norma.podrocje)
  return(norma)
}

neskoncna.norma2 <- function(center, tocka,max.cena, max.velikost, vrednost.podrocje){
  norma.cena <- abs(as.numeric(tocka[1,8]) - as.numeric(center[1,1])/max.cena)^2
  norma.velikost <- abs(as.numeric(tocka[1,7]) - as.numeric(center[1,2])/max.velikost)^2
  norma.podrocje <- vrednost.podrocje
  if(identical(as.character(tocka[1,4]),as.character(center[1,3]))){
    norma.podrocje <- 0
  }
  norma <- max(norma.cena, norma.velikost, norma.podrocje)
  
  return(norma)
}
# 4. Reševanje linearnega programa:########################################################################
poisci.resitve2 <- function(tocke2, vsi.centri2, zeljene.utezi2, utezi2, norma ,povprecje.cena, povprecje.velikost, konstanta){ 
  resitve2 <- Linearni.program(tocke2, vsi.centri2, zeljene.utezi2, utezi2, norma ,povprecje.cena, povprecje.velikost, konstanta)
  #View(resitve)
  
  # 5. Razvrščanje točk v gnezde glede na rešitev linearnega programa:#####################################
  razdelitev <- function(resitve2){
    gnezda <- rep(0,ncol(resitve2))
    for(i in 1:ncol(resitve2)){
      gnezdo <- as.vector(which(resitve2[,i] != 0))
      gnezda[i] <- paste("gnezdo ", as.character(gnezdo), sep = "")[1]
    }
    return(gnezda)
  }
  
  gnezda2 <- razdelitev(resitve2)
  gnezdene.tocke2 <- cbind(tocke2, gnezda2)
  gnezdene.tocke2 <- gnezdene.tocke2[order(gnezdene.tocke2$gnezda2),]
  return(gnezdene.tocke2)
}
# 6. Grafični prikaz rešitev:#############################################################################

# Funkcija, ki dane rešitve izriše na grafu 2D dimenzije, oziroma 3D dimenzije:


izberi.graf2 <- function(input,izbrane.tocke,vsi.centri){
  if(input == "2D"){
    graf <- plot_ly() %>% 
      add_trace(data=izbrane.tocke, x= ~podrocje, y= ~cena, 
                color = ~gnezda2, type = 'scatter',
                marker = list(size = 5)) %>%
      add_trace(data=vsi.centri, x= ~podrocje.centri,y= ~cena.centri, type = 'scatter',
                mode = 'text', text = ~imena.gnezd, textposition = 'middle right',
                textfont = list(color = "black", size = 12),
                marker = list(size = 5, color = 'black',symbols = 'x')) %>%
      layout(title = 'Razporeditev tock v gnezda')

  }else{
    graf <- plot_ly() %>% 
      add_trace(data=izbrane.tocke, x= ~cena, y= ~velikost, 
                z= ~podrocje, color = ~gnezda2,
                marker = list(size = 5)) %>%
      add_trace(data=vsi.centri, x= ~cena.centri, y= ~velikost.centri, 
                z= ~podrocje.centri,
                mode = 'text', text = ~imena.gnezd, textposition = 'middle right',
                textfont = list(color = "black", size = 12),
                marker = list(size = 5,color = 'black',symbols = 'x')) %>%
      layout(title = 'Razporeditev tock v gnezda')
  }
  return(graf)
}
