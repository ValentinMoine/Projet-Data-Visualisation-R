
#Package
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)
library(ggrepel)
library(waffle)
library(extrafont)
library(FactoMineR)
library(factoextra)
library(purrr)
library(leaflet)
library(magrittr)
library(sp)
library(rgdal)
library(devtools)
library(gridExtra)
library(geojsonio)
library(leaflet)
library(ggiraph)
library(maps)
library(fmsb)
library(forcats)

# On importe les donnees
dataframe = tbl_df(read_csv("COVID_data_2020-05-08.csv"))


function(input, output) {

  #___________________________Comparaison de deux pays sur une valeur donnee_________________________________________
  
  
  # mise en forme des datasets, pour avoir les informations des deux pays qui nous interessent
  
  temp_comp1 <- reactive({
    subset(dataframe, dataframe$country==input$pays1)
  })
  
  temp_comp2 <- reactive({
    subset(dataframe, dataframe$country==input$pays2)
  })
  
  temp_comp3 <- reactive({
    bind_rows(temp_comp1(), temp_comp2())
  })
  
  
  # De meme avec les differents types de donnee
  
  temp_comp4 <- reactive({
    if (input$outcome_select=="Cas (total)") { 
      outcome = temp_comp3()$cases
    }
    if (input$outcome_select=="Deces (total)") { 
      outcome = temp_comp3()$deaths 
    }
    if (input$outcome_select=="Cas actifs (total)") { 
      outcome = temp_comp3()$active_cases
    }
    if (input$outcome_select=="Individus gueries (total)") { 
      outcome = temp_comp3()$recovered
    }
    if (input$outcome_select=="Cas pour 100 000 habitants") { 
      outcome = temp_comp3()$per100k
    }
    if (input$outcome_select=="Deces pour 100 000 habitants") { 
      outcome = temp_comp3()$deathsper100k
    }
    if (input$outcome_select=="Cas actifs pour 100 000 habitants") { 
      outcome = temp_comp3()$activeper100k
    }
    mutate(temp_comp3(),outcome=outcome)
  })
  
  #On prepare le dataset au bon format pour le diagramme
  DataComp <- reactive({
    data.frame(country=temp_comp4()$country,outcome=temp_comp4()$outcome, date = temp_comp4()$date)
  })
  
  #affichage du dataset
  output$DataComp <- renderTable({
    DataComp()
  },rownames = TRUE)
  
  #Affichage du diagramme
  output$lignPlot <- renderPlot({
      ggplot(data = DataComp()) +
        geom_smooth(mapping = aes(x = date, y = outcome, color = country))
  })

  
  
  
  
  
  

#_____________________________________ Etat d'un pays sur une date donnee _______________________________________________
 
  
  
  
  # mise en forme des datasets, pour avoir les informations a la date qui nous interesse
  
  inter_bars1 <- reactive({
    subset(dataframe, dataframe$date == input$date_comp)
  })
  
  
  # De meme avec les differents types de donnee
  
  inter_bars2 <- reactive({
    if (input$outcome_select_comp=="Cas (total)") { 
      outcome = inter_bars1()$cases
    }
    if (input$outcome_select_comp=="Deces (total)") { 
      outcome = inter_bars1()$deaths 
    }
    if (input$outcome_select_comp=="Cas actifs (total)") { 
      outcome = inter_bars1()$active_cases
    }
    if (input$outcome_select_comp=="Individus gueris (total)") { 
      outcome = inter_bars1()$recovered
    }
    if (input$outcome_select_comp=="Cas pour 100 000 habitants") { 
      outcome = inter_bars1()$per100k
    }
    if (input$outcome_select_comp=="Deces pour 100 000 habitants") { 
      outcome = inter_bars1()$deathsper100k
    }
    if (input$outcome_select_comp=="Cas actifs pour 100 000 habitants") { 
      outcome = inter_bars1()$activeper100k
    }
    mutate(inter_bars1(), outcome=outcome)
  })
  
  #reorganisation des donnees pour qu'elles soient dans l'ordre
  data_bars <- reactive({
    mutate(inter_bars2(), country = fct_reorder(country, outcome))
  })
  
  #selection des 20 premiers pays
  data_bars_final <- reactive({
    slice(data_bars(), 1:20)
  })
  
  #affichage du dataset
  output$data_bars_final <- renderTable({
    data_bars_final()
  },rownames = TRUE)
  
  #affichage du graph
  output$geombars <- renderPlot({
    ggplot(data_bars_final(), 
           aes(x = country, y = outcome), 
           fill = country)+ 
      geom_bar(
        stat = 'identity', 
        width = 0.8
      )+
      coord_flip()+
      theme(legend.position = "none")

  })
  
  
  
  
  
  
  
  #____________________________________Superposition des informations________________________________
  
  
  
  
  # mise en forme des datasets, pour avoir les informations a la date qui nous interesse
  
  temp1_araignee <- reactive({
    subset(dataframe, dataframe$date == input$date_etat)
  })
  
  
  # mise en forme des datasets, pour avoir les informations des deux pays qui nous interessent
  
  data_country1 <- reactive({
    subset(temp1_araignee(), temp1_araignee()$country == input$country1) %>%
      select(-X1, -country, -date)
  })
  
  data_country2 <- reactive({
    subset(temp1_araignee(), temp1_araignee()$country == input$country2) %>%
      select(-X1, -country, -date)
  })
  
  
  #nettoyage des donnees
  #recuperation du min et max pour pouvoir creer le radar chart
  
  data_max <- reactive({
    subset(dataframe, dataframe$date == input$date_etat) %>%
      filter(!is.na(per100k))%>%
      filter(!is.na(newper100k))%>%
      filter(!is.na(activeper100k))%>%
      filter(!is.na(deathsper100k))%>%
      filter(!is.na(newdeathsper100k))%>%
      select(-X1, -country, -date) %>%
      summarise_each(funs(max))
  })
  
  data_min <- reactive({
    subset(dataframe, dataframe$date == input$date_etat) %>%
      filter(!is.na(per100k)) %>%
      filter(!is.na(newper100k)) %>%
      filter(!is.na(activeper100k)) %>%
      filter(!is.na(deathsper100k)) %>%
      filter(!is.na(newdeathsper100k)) %>%
      select(-X1, -country, -date) %>%
      summarise_each(funs(min))
  })
  
  #mise en forme du dataset pour pouvoir creer le graphique

  data_araignee <- reactive({
    bind_rows(data_max(),data_min()) %>%
    bind_rows(data_country1()) %>%
    bind_rows(data_country2())
  })
  
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
  lengend <- reactive({
    bind_rows(data_country1(), data_country2())
  })
                   
  # affichage du dataset        
  output$data_araignee <- renderTable({
    data_araignee()
  },rownames = TRUE)
  
  #affichage du graph
  output$araignee <- renderPlot(
    radarchart(data_araignee(),
               axistype = 1,
               
               #custom polygon
               pcol=colors_border, 
               pfcol=colors_in, 
               plwd=4, 
               plty=1,
               
               #custom the grid
               cglcol="grey", 
               cglty=1, 
               axislabcol="grey", 
               cglwd=0.8,
               #custom labels
               vlcex=0.8
               )
  )
  
  
  
}





























