# 1. Primer: 


# 1. Uvoz podatkov:##########################################################################################

stanovanja.tipi <- read.csv("eksperimentStanovanja\\tipi.csv")
stanovanja.ostalo <- read.csv("eksperimentStanovanja\\stanovanja.csv")

stanovanja.podatki <- merge(stanovanja.tipi,stanovanja.ostalo,by="id")
stanovanja.podatki <- stanovanja.podatki[order(stanovanja.podatki$velikost),]
head(stanovanja.podatki) # Prikaz oblike uvoženih podatkov

# 2. Oblikovanje optimizacijskega problema:##################################################################

tocke <- data.frame(stanovanja.podatki)
m <- nrow(stanovanja.podatki) # št. točk
k <- 18 # št. gnezd na katere gelimo točke
utezi <- rep(1, m) # vse točke otežimo z 1

# Gnezda imajo enakomerne utezi, ce jih ni mozno celostevilko razdeliti na k delov, 
# ostanek dobi srednje gnezdo (,ki bo imelo kasneje točke z najpogostejšimi vrednostmi)
zeljene.utezi <- rep((m %/% k), k)
zeljene.utezi[k %/% 2] <- zeljene.utezi[k %/% 2] + m %% k

# Definicija centrov:
# Centri glede na ceno...x koordinata centrov
povprecje.cena <- mean(as.numeric(stanovanja.podatki$cena))
odklon.cena <- sd(as.numeric(stanovanja.podatki$cena))
centri.cena <- rep(0, 6)
for( i in 0:2){
  centri.cena[3 - i] <- (povprecje.cena - 50000 + (odklon.cena*i^(8/7)*0.6))
  centri.cena[4 + i] <- (povprecje.cena - 70000 - (odklon.cena*i*0.3))
}

# Centri glede na velikost...y koordinata centrov
povprecje.velikost <- mean(as.numeric(stanovanja.podatki$velikost))
odklon.velikost <- sd(as.numeric(stanovanja.podatki$velikost))
centri.velikost <- c((povprecje.velikost - odklon.velikost * 0.8), povprecje.velikost + odklon.velikost*0.8 , 
                     (povprecje.velikost + odklon.velikost*2.2))

# Centri glede na podrocja...z koordinata centrov
razlicna.podrocja <- c("Bezigrad","Center", "Moste-Polje","Siska","Vic-Rudnik")
centri.podrocje <- rep(razlicna.podrocja, (k %/% 5))
if((k %% 5) != 0){
  for(i in 1:(k %% 5)){
    centri.podrocje <- c(centri.podrocje, razlicna.podrocja[i])
  }
}

# Združevanje (vse tri koordinate) in popravljanje centrov, da bodo kar najbolj smislno pokrivali točke:
vsi.centri <- expand.grid(centri.cena, centri.velikost)  %>% cbind(centri.podrocje)
colnames(vsi.centri) <- c("cena.centri", "velikost.centri","podrocje.centri")
vsi.centri <- vsi.centri[order(vsi.centri$velikost.centri),]
vsi.centri[13:18,1] <- vsi.centri[13:18,1] + 50000
vsi.centri[8:12,1] <- vsi.centri[8:12,1] + 25000
vsi.centri <- vsi.centri[order(vsi.centri$cena.centri),]
vsi.centri[16:18,2] <- vsi.centri[16:18,2] + 60
vsi.centri[16:18,1] <- vsi.centri[16:18,1] + 50000
vsi.centri[1:3,2] <- vsi.centri[1:3,2]/2.5
vsi.centri[4:6,2] <- vsi.centri[4:6,2]/2
vsi.centri[7:12,2] <- vsi.centri[7:12,2]/1.5

# Centrom dodelimo imena, ki so enaka številom od 1-k:
imena.gnezd <- c()
for(i in 1:k){
  imena.gnezd[i] <- as.character(i)
}
centri <- cbind(vsi.centri,imena.gnezd)

max.cena <- max(tocke$cena)
max.velikost <- max(tocke$velikost)

# Definiranje razdalje:
druga.norma <- function(center, tocka,max.cena, max.velikost, vrednost.podrocje){
  norma.cena <- abs(as.numeric(tocka[1,8]) - as.numeric(center[1,1])/max.cena)^2
  norma.velikost <- abs(as.numeric(tocka[1,7]) - as.numeric(center[1,2])/max.velikost)^2
  norma.podrocje <- vrednost.podrocje
  if(identical(as.character(tocka[1,4]),as.character(center[1,3]))){
    norma.podrocje <- 0
  }
  norma <- (norma.cena + norma.velikost + norma.podrocje)
  
  
  return(norma)
}

neskoncna.norma <- function(center, tocka,max.cena, max.velikost, vrednost.podrocje){
  norma.cena <- abs(as.numeric(tocka[1,8]) - as.numeric(center[1,1])/max.cena)^2
  norma.velikost <- abs(as.numeric(tocka[1,7]) - as.numeric(center[1,2])/max.velikost)^2
  norma.podrocje <- vrednost.podrocje
  if(identical(as.character(tocka[1,4]),as.character(center[1,3]))){
    norma.podrocje <- 0
  }
  norma <- max(norma.cena, norma.velikost, norma.podrocje)
  
  return(norma)
}


# 4. Reševanje linearnega programa:##########################################################################


poisci.resitve1 <- function(tocke, centri, zeljene.utezi, utezi, norma, max.cena, max.velikost, konstanta){
  # Spodnja funkcija je definirana v datoteki LinearniProgram.R
  resitve <- Linearni.program(tocke, centri, zeljene.utezi, utezi, norma, max.cena, max.velikost, konstanta)
  
  # 5. Razvrščanje točk v gnezde glede na rešitev linearnega programa:#########################################
  
  razdelitev <- function(resitve){
    gnezda <- rep(0,ncol(resitve))
    for(i in 1:ncol(resitve)){
      gnezdo <- as.vector(which(resitve[,i] > 0))
      gnezda[i] <-  paste("gnezdo ", as.character(gnezdo), sep = "")[1]
    }
    return(gnezda)
  }
  
  gnezda <- razdelitev(resitve)
  gnezdene.tocke <- cbind(tocke, gnezda)
 return(gnezdene.tocke) 
}


# 6. Grafični prikaz rešitev:################################################################################

# Funkcija, ki dane rešitve izriše na grafu 2D dimenzije, oziroma 3D dimenzije:

izberi.graf1 <- function(input,izbrane.tocke,centri){
  if(input == "2D"){
    graf <- plot_ly() %>% 
      add_trace(data=izbrane.tocke, x= ~cena, y= ~velikost, color = ~gnezda,
                marker = list(size = 5)) %>% 
      add_trace(data=centri, x= ~cena.centri, y= ~velikost.centri,type = 'scatter',
                mode = 'text', text = ~imena.gnezd, textposition = 'middle right',
                textfont = list(color = "black", size = 12),
                marker = list(size = 5, color = 'black',symbols = 'x')) %>% 
    layout(title = 'Razporeditev tock v gnezda')
      
  }else{
    graf <- plot_ly() %>% 
      add_trace(data=izbrane.tocke, x= ~cena, y= ~velikost, 
                z= ~podrocje, color = ~gnezda,
                marker = list(size = 5)) %>% 
      add_trace(data=centri, x= ~cena.centri, y= ~velikost.centri, 
                z= ~podrocje.centri,type = 'scatter3d',
                mode = 'text', text = ~imena.gnezd, textposition = 'middle right',
                textfont = list(color = "black", size = 12),
                marker = list(size = 5,
                              color = 'black',symbols = 'x')) %>%
    layout(title = 'Razporeditev tock v gnezda')
  }
  return(graf)
}

