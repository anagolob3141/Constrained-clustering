# Amplikacija z analizo rešitev, omogoča spreminjanje izbirnih začetnih parametrov.

# Uvoz vseh potrebnih knjižnjic:
library(shiny)
library(ggplot2)
library(plotly)
library(lpSolveAPI)
source("eksperimentStanovanja\\LinearniProgramStanovanja.R")
source("eksperimentStanovanja\\primer1.R")
source("eksperimentStanovanja\\primer2.R")

# Konstrukcija možnih izbir za checkboxGroupInput: 
izbire <- list("Gnezdo 1" = "gnezdo 1","Gnezdo 2" = "gnezdo 2","Gnezdo 3" = "gnezdo 3",
               "Gnezdo 4" = "gnezdo 4","Gnezdo 5" = "gnezdo 5","Gnezdo 6" = "gnezdo 6",
               "Gnezdo 7" = "gnezdo 7","Gnezdo 8" = "gnezdo 8","Gnezdo 9" = "gnezdo 9",
               "Gnezdo 10" = "gnezdo 10","Gnezdo 11" = "gnezdo 11","Gnezdo 12" = "gnezdo 12",
               "Gnezdo 13" = "gnezdo 13","Gnezdo 14" = "gnezdo 14","Gnezdo 15" = "gnezdo 15",
               "Gnezdo 16" = "gnezdo 16","Gnezdo 17" = "gnezdo 17","Gnezdo 18" = "gnezdo 18")


ui <- fluidPage(
  
  titlePanel("Grupiranje stanovanj razpoložljivih za prodajo"),
  
  sidebarLayout(

    sidebarPanel(
      
      #Izbira dimenzije grafa
      selectInput("tip", "Dimenzija grafa", 
                  choices = c("2D", "3D"), selected = "2D"),
      
      #Izbira norme
      selectInput("norma", "Kako je definirana razdalja:", 
                  choices = c("Druga norma", "Neskončna norma"), selected = "Druga norma"), 
      
      #Izbira konstante, ki je uporabljena v definiciji diskretne norme pri področjih
      sliderInput("konstanta", "Kako občutljiva je norma glede na področje:",
                  min = 0.4, max = 2,
                  value = 1),         
      
      #Izbira možnosti, da se na grafu ne prikažejo podatki za vsa gnezda:
      checkboxGroupInput("gnezdo", "Gnezda prikazana na grafu:", 
                         choices = izbire, selected = izbire),
      
      tableOutput("data")
      
    ),
    
    
    

    mainPanel(
      
      #Izbira enega izmed dveh primerov, ki smo ju konstruirali:
      tabsetPanel(type = "tabs",
                  tabPanel("Primer 1",plotlyOutput(outputId = "gra1")),
                  tabPanel("Primer 2", plotlyOutput(outputId = "gra2"))),
      textOutput("opis")
      
    )
  )
)


server <- function(input, output) {
  
  output$gra1 <- renderPlotly({
    
    if(input$norma == "Druga norma"){
      norma <- druga.norma
    }else{
      norma <- neskoncna.norma
    }
      gnezdene.tocke <- poisci.resitve1(tocke, centri, zeljene.utezi, utezi,
                                        norma, max.cena, max.velikost, input$konstanta)
      izbrane.tocke <- subset(gnezdene.tocke, gnezda %in% input$gnezdo)
      izberi.graf1(input$tip,izbrane.tocke,centri)
    
  })
  
  output$gra2 <- renderPlotly({
    
    if(input$norma == "Druga norma"){
      norma <- druga.norma2
    }else{
      norma <- neskoncna.norma2
    }
    
    gnezdene.tocke2 <- poisci.resitve2(tocke2, vsi.centri2, zeljene.utezi2, utezi2, 
                                       norma ,povprecje.cena, povprecje.velikost, input$konstanta)
    izbrane.tocke2 <- subset(gnezdene.tocke2, gnezda2 %in% input$gnezdo)
    izberi.graf2(input$tip,izbrane.tocke2,vsi.centri2)
    
  })
  
 
  output$opis <- renderText({ 
    "Črne točke na grafu prikazijejo centre gruč. 
    Število, zapisano ob točkah sporoča za center katere gruče gre.\n
    Če boste ob robu spreminjali parametre, lahko traja nekaj časa,
    da program izriše novo rešitev. Hitreje bo, če spreminjajte po en parameter na enkrat.
    Zgoraj lahko izbirate med dvema primeroma, razlikujeta se po postavitvi centrov gruč."
  }) 
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)