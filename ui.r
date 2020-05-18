

#package
library(shiny)
library(readr)
library(shinyWidgets)
library(dplyr)
library(leaflet)

# Importation des donnees
dataframe = tbl_df(read_csv("COVID_data_2020-05-08.csv"))

liste_pays = sort(unique(subset(dataframe)$country))

bootstrapPage(

  navbarPage(title = "Countries elements of comparison on COVID19 data",

    theme = shinythemes::shinytheme("cyborg"),
    
    
    
    #_________________________grpahique 1_________________________
    
    
    tabPanel("Compare two countries",
       sidebarLayout(
         sidebarPanel(
           pickerInput("pays1", "Pays 1 :",   
                       choices = c(liste_pays), 
                       selected = c("USA"),
                       multiple = FALSE),
           pickerInput("pays2", "Pays 2 :",   
                       choices = c(liste_pays), 
                       selected = c("France"),
                       multiple = FALSE),
           
           pickerInput("outcome_select", "measure unit :",   
                       choices = c("Deces pour 100 000 habitants", "Cas pour 100 000 habitants", "Cas actifs pour 100 000 habitants","Cas (total)", "Deces (total)","Cas actifs (total)", "Individus gueris (total)"), 
                       selected = c("Cas (total)"),
                       multiple = FALSE)
           
           
         ),
         
         
         mainPanel(
           
           titlePanel("Country Comparison"),
           tabsetPanel(type = "tabs",
                       tabPanel("Comparison", plotOutput("lignPlot")),
                       tabPanel("Data Table",
                                tableOutput("DataComp"))
                       
                       
           )
         )
      )
    ),
    
    
    
    
    #______________________________graphique 2____________________________________________________
    
    tabPanel("Country Ranking",
             
             sidebarLayout(
               sidebarPanel(
                 
                 
                sliderInput("date_comp", "Date :",
                             min = as.Date("2020-02-01","%Y-%m-%d"), max = as.Date("2020-05-08","%Y-%m-%d"), 
                             step=1, timeFormat = "%Y-%m-%d",
                             value = as.Date("2020-05-08","%Y-%m-%d")),
               
                pickerInput("outcome_select_comp", "Value to compare :",   
                           choices = c("Deces pour 100 000 habitants", "Cas pour 100 000 habitants", "Cas actifs pour 100 000 habitants","Cas (total)", "Deces (total)","Cas actifs (total)", "Individus gueries (total)"), 
                           selected = c("Cas (total)"),
                           multiple = FALSE)
               ),
               
               mainPanel(
                 
                 titlePanel("Country Ranking"),
                 tabsetPanel(type = "tabs",
                             tabPanel("Ranking", plotOutput("geombars")),
                             tabPanel("Data Table",
                                      tableOutput("data_bars_final"))
                             
                             
                 )
               )
             )
    ),
    
    
    
    #___________________________________________Graphique 3_______________________________________________________
    
    tabPanel("Country data superposition",
             sidebarLayout(
               sidebarPanel(
                 
                 
                 sliderInput("date_etat", "Date selection :",
                             min = as.Date("01-02-2020","%d-%m-%Y"), max = as.Date("08-05-2020","%d-%m-%Y"), 
                             step=1, timeFormat = "%d-%m-%Y",
                             value = as.Date("08-05-2020","%d-%m-%Y")),
                 
                 pickerInput("country1", "First Country :",   
                             choices = c(liste_pays), 
                             selected = c("France"),
                             multiple = FALSE),
                 
                 pickerInput("country2", "Second Country :",   
                           choices = c(liste_pays), 
                           selected = c("USA"),
                           multiple = FALSE)
                          ),
               
               mainPanel(
                 
                 titlePanel("Country State"),
                 tabsetPanel(type = "tabs",
                             tabPanel("Radar Chart", plotOutput("araignee")),
                             tabPanel("Data Table",
                                      tableOutput("data_araignee"))
                 
                 
                            )
                        )
                    )
                )
          )
    )

