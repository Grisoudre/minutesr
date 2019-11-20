# Packages, options et sources ---------------

# source timer : https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny

library(lubridate)
library(shiny)
library(questionr)
library(tidyverse)
library(shinythemes)

# GLOBAL ---------------
# tpt <- data.frame(id="0", nom="", genre="",statut="",temps="", stringsAsFactors = F)

GetTableMetadata <- function() {
  fields <- c(id = "id", reunion = "reunion",
              nom = "nom", genre = "genre", statut = "statut",
              temps = "temps" 
  )
  
  result <- list(fields = fields)
  return (result)
}

GetNextId <- function() {
  if (exists("tpt") && nrow(tpt) > 0) {
    max(as.integer(rownames(tpt))) + 1
  } else {
    return (1)
  }
}


CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("tpt")) {
    tpt <<- rbind(tpt, data)
  } else {
    tpt <<- data
  }
}


ReadData <- function() {
  if (exists("tpt")) {
    tpt
  }
}


UpdateData <- function(data) {
  data <- CastData(data)
  tpt[row.names(tpt) == row.names(data), ] <<- data
}


DeleteData <- function(data) {
  tpt <<- tpt[row.names(tpt) != unname(data["id"]), ]
}


CastData <- function(data) { 
  datar <- data.frame( reunion = data["reunion"],
                       nom = data["nom"],
                       genre = data["genre"],
                       statut = data["statut"],
                       temps = data["temps"],
                       stringsAsFactors = FALSE) 
  
  rownames(datar) <- data["id"]
  return (datar)
}


CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0",
                             reunion="",
                             nom="",
                             genre="",
                             statut="",
                             temps="" ))
  
  return (mydefault)
}


UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "reunion", value = unname(data["reunion"]))
  updateTextInput(session, "nom", value = unname(data["nom"]))
  updateSelectInput(session, "genre", selected = unname(data["genre"]))
  updateSelectInput(session, "statut",selected = unname(data["statut"]))
  updateTextInput(session, "temps", value = unname(data["temps"]))
}

# UI ----------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),theme=shinytheme("united"),
  h1("Minut'ESR"),
  fluidRow(
    column(6,
           fluidRow(
             column(6,
                    shinyjs::disabled(textInput("id", "Id", "0")),
                    textInput("reunion", label = h4("Réunion"), value = ""),
                    h3("Minuteur :"),
                    wellPanel(
                      actionButton('start','Go',
                                   style="color:SeaGreen;font-weight: bold;background-color:MediumAquaMarine  ;"),
                      actionButton('stop','Stop',
                                   style="color:DarkGoldenRod ;font-weight: bold;
                                   background-color:Khaki   ;"),
                      actionButton('reset','Reset',
                                   style="color:red ;font-weight: bold;
                                   background-color:pink;"),
                      shinyjs::disabled(uiOutput("tempsUI"))
                      )),
             column(6,
                    h3("Qui parle :"),
                    textInput("nom", label = h4("Nom"), value = ""),
                    radioButtons("genre", label = h4("Genre"),
                                 choices = list("Femme", "Homme"), 
                                 selected = "Femme"),
                    radioButtons("statut", label = h4("Statut"),
                                 choices = list("Titulaire", "Non-titulaire"), 
                                 selected = "Titulaire"))
             ),
           fluidRow(
             
             #action buttons
             actionButton("submit", "Enregistrer",
                          style="color:SeaGreen;font-weight: bold;background-color:MediumAquaMarine  ;"),
             actionButton("delete", "Supprimer",
                          style="color:red ;font-weight: bold;
                          background-color:pink   ;"),
             actionButton("new", "Nouveau",
                          style="color:DarkGoldenRod ;font-weight: bold;
                          background-color:Khaki;}"),
             DT::dataTableOutput("tpt"),
             
             downloadButton('dl', 'Télécharger',class = "Down")
             )),
    column(6, 
           column(6,h3("Nombre d'interventions"),
                  h4("- Maximum :"),
                  textOutput("nbMax"),
                  h4("- Par genre :"),
                  tableOutput("nbGenre"),
                  h4("- Par statut :"),
                  tableOutput("nbStatut"),
                  h4("- Pourcentages selon le genre :"),
                  tableOutput("nbPercGenre"),
                  h4("- Pourcentages selon le statut :"),
                  tableOutput("nbPercStatut"),
                  h4("- Pourcentages globaux :"),
                  tableOutput("nbPercAll")),
           column(6,h3("Durées des interventions"),
                  h4("- Maximum :"),
                  textOutput("tpsMax"),
                  h4("- Par genre :"),
                  tableOutput("tpsGenre"),
                  tableOutput("tpsPercGenre"),
                  tableOutput("tpsPercGenreS"),
                  h4("- Par statut :"),
                  tableOutput("tpsStatut"),
                  tableOutput("tpsPercStatut"),
                  tableOutput("tpsPercStatutG"),
                  h4("- Par genre et statut :"),
                  tableOutput("tpsAll"),
                  tableOutput("tpsPercAll")))
           )
  
  
  )

# SERVEUR ------------------

server <- function(input, output, session) {
  
  # Données de départ :
  timer <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  
  duree_txt <- reactive({
    seconds_to_period(timer())
  })
  output$tempsUI <- renderUI({
    textInput("temps", "Durée :", duree_txt())
    
  })
  
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$tpt_rows_selected, {
    if (length(input$tpt_rows_selected) > 0) {
      data <- ReadData()[input$tpt_rows_selected, ]
      UpdateInputs(data, session)
      
    }
    
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
      
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
    
    shinyjs::click("stop")
    shinyjs::click("reset")
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    
    UpdateInputs(CreateDefaultRecord(), session)
    
    shinyjs::click("stop")
    shinyjs::click("reset")
  })
  
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # table
  output$tpt <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields),
  extensions = c('Scroller','FixedColumns'), options = list(
    deferRender = F,
    scrollY = 200,
    scrollX = TRUE,
    fixedColumns = TRUE, scroller = TRUE)
  )   
  
  
  # Durée :
  output$duree <- renderText({
    paste("Durée : ", as.character(seconds_to_period(timer())))
  })
  
  # Comportement :
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()+1)
      }
    })
  })
  
  # Boutons :
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(0)})
  
  # Télécharger :
  
  
  output$dl <- downloadHandler(
    filename = function() {
      paste('TimerESR_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv2(tpt, con, na="")
    }
  )
  
  # Calculs :
  output$nbMax <- reactive({
    input$submit
    input$delete
    tpt <- ReadData()
    validate (need(is.null(tpt)==F, ""))
    a<-group_by(tpt, nom) %>% 
      summarise(max = max(n(),na.rm=T)) %>% 
      filter(nom!="") %>% 
      select(max) %>% arrange(desc(max)) %>%  data.frame() 
    a[1,1]
  }) 
  
  
  output$nbGenre <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    freq(tpt$genre, sort="dec",valid=F, total=T)
  }, rownames=T)
  
  output$nbStatut <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    freq(tpt$statut, sort="dec",valid=F, total=T)
  }, rownames=T)
  
  output$nbPercGenre <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    a<- rprop(table(tpt$genre, tpt$statut, useNA ="ifany"))%>% 
      data.frame() %>% 
      spread(Var2,Freq) %>% 
      rename(Genre=Var1)
    for (i in (2:ncol(a))){
      a[,i] <- round(a[,i],1)
    }
    a
  }, rownames=F)
  
  output$nbPercStatut <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    a<-  rprop(table(tpt$statut,tpt$genre,  useNA ="ifany"))%>% 
      data.frame() %>% 
      spread(Var2,Freq) %>% 
      rename(Statut=Var1)
    for (i in (2:ncol(a))){
      a[,i] <- round(a[,i],1)
    }
    a
  }, rownames=F)
  
  output$nbPercAll <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    a<- prop(table(tpt$statut,tpt$genre,  useNA ="ifany"))%>% 
      data.frame() %>% 
      spread(Var2,Freq) %>% 
      rename(Statut=Var1)
    for (i in (2:ncol(a))){
      a[,i] <- round(a[,i],1)
    }
    a
  }, rownames=F)
  
  
  output$tpsMax <- reactive({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    a<-group_by(tpt) %>% 
      summarise(max = max(tempst, na.rm=T)) %>% 
      select(max) %>% data.frame() 
    as.character(seconds_to_period(a[1,1]))
    
  }) 
  
  output$tpsStatut <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>%
      group_by(statut) %>%
      summarise(`Temps moyen`=as.character(seconds_to_period(round(mean(tempst, na.rm=T),1))),
                `Temps médian`=as.character(seconds_to_period(round(median(tempst, na.rm=T),1))) )
    
    
  }) 
  output$tpsPercStatut <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>% mutate(Total=sum(tempst, na.rm=T)) %>%
      group_by(statut, Total)  %>% 
      summarise(Temps=round(sum(tempst, na.rm=T),1)) %>% 
      transmute(PourcTemps = round(Temps/Total*100,1))
    
  }) 
  output$tpsPercStatutG <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>% group_by(statut) %>% mutate(Total=sum(tempst, na.rm=T)) %>%
      group_by(statut,genre, Total)  %>% 
      summarise(Temps=round(sum(tempst, na.rm=T),1)) %>% 
      transmute(PourcTemps = round(Temps/Total*100,1)) %>% spread(genre, PourcTemps) %>% 
      mutate(Total = 100)
    
  }) 

  output$tpsGenre <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>%
      group_by(genre) %>%
      summarise(`Temps moyen`=as.character(seconds_to_period(round(mean(tempst, na.rm=T),1))),
                `Temps médian`=as.character(seconds_to_period(round(median(tempst, na.rm=T),1))) )
  }) 
  output$tpsPercGenre <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>% mutate(Total=sum(tempst, na.rm=T)) %>%
      group_by(genre, Total)  %>% 
      summarise(Temps=round(sum(tempst, na.rm=T),1)) %>% 
      transmute(PourcTemps = round(Temps/Total*100,1))
    
  }) 
  
  output$tpsPercGenreS <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    
    validate (need(is.null(tpt)==F, ""))
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>% group_by(genre) %>% mutate(Total=sum(tempst, na.rm=T)) %>%
      group_by(genre, statut,Total)  %>% 
      summarise(Temps=round(sum(tempst, na.rm=T),1)) %>% 
      transmute(PourcTemps = round(Temps/Total*100,1)) %>% spread(statut, PourcTemps) %>% 
      mutate(Total = 100)
    
  }) 
  output$tpsAll <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    validate (need(is.null(tpt)==F, ""))
    
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>%
      group_by(statut, genre) %>%
      summarise(`Temps moyen`=as.character(seconds_to_period(round(mean(tempst, na.rm=T),1))),
                `Temps médian`=as.character(seconds_to_period(round(median(tempst, na.rm=T),1))) )
  }) 
  output$tpsPercAll <- renderTable({
    input$submit
    input$delete
    tpt <- ReadData()
    validate (need(is.null(tpt)==F, ""))
    
    tpt$tempst <- period_to_seconds(as.period(tpt$temps))
    tpt %>% mutate(Total=sum(tempst, na.rm=T)) %>%
      group_by(genre,statut, Total)  %>% 
      summarise(Temps=round(sum(tempst, na.rm=T),1)) %>% 
      transmute(PourcTemps = round(Temps/Total*100,1)) %>% spread(statut,PourcTemps)
  }) 
  
}

shinyApp(ui = ui, server = server)
