#
# This is a Shiny web application
# Made by Lucca Nielsen
#
# Github Project:https://github.com/Luccan97/StandardizedMortality_Brazil


# Packages
library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(sf)
library(bslib)


# Reading the cleaned datasets from github repo  

# deaths
obts_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/obts_clean.csv", 
                       col_types = cols(Sex = col_character()), 
                       locale = locale(encoding = "ISO-8859-1"))

# The shp file needs a diferent process, we need to download them first...
githubURL <- ("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/UF_shp.RDS")
download.file(githubURL,"UF_shp.rds", method="curl")
UF_shp <- readRDS("UF_shp.rds")



# population
pop_t_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/pop_t_clean.csv")

# standard population
standard_pop_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/standard_pop_clean.csv")


ui <- fluidPage(
  # overall theme
  theme = bs_theme(bootswatch = 'yeti'),
  
  
  h1(id="tag1", "Open data <> Free knowledge <> Public Health."),
  
  h1(id="tag2", "Standardized mortality rate by sex and age group in the States of Brazil (2010-2019)."),

  tags$head(
    tags$style(HTML(
      
      # CSS styling some features
      "#
      plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
      
      
      #tag1 {color: white;
                  background-color:#405d27;
                  border:2px solid #c1946a;
                  border-radius: 15px 50px 30px;
                  font-size: 20px;
                  font-style: bold;
                  text-align:right;
                  padding-top:10px;
                  padding-bottom:10px;
                  
                  }
    #tag2 {color: #c1946a;
                  background-color: #405d27;
                  border-radius: 15px 50px 30px;
                  border:2px solid #c1946a;
                  font-size: 25px;
                  font-style: bold;
                  text-align:center;
                  padding-top:10px;
                  padding-bottom:10px;
                 
    }
    .tabbable > .nav > li > a[data-value='Map']
      {background-color: #405d27;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
   
    }
    .tabbable > .nav > li > a[data-value='Data']
      {background-color: #405d27;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-size:20px;
    
      }
      .tabbable > .nav > li > a[data-value='README']
      {background-color: white;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
    
      }
    
  "))),

  # Sidebar with brief description of dashboard utilities and input choices
    sidebarLayout(
      
        sidebarPanel(
          width = 6,
          h4("What is it and how to use it?"),
          helpText("The dashboard offers an interactive visualization of the spatial distribution in the Federative Units of Brazil of the mortality rates standardized by sex and age group,
                   according to the basic causes grouped in the chapters of the ICD-10.
                   To learn more about the build process, click on the 'READme' tab of the panel.
                  
                   To create a specific map:
                   Select a year, rate type (standardized or crude), and the ICD-10 chapter of interest."),
          selectInput("year",
                      "Year:", 
                      selected = "2019",
                      choices = c('2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')),
          selectInput("taxa",
                      "Rate:",
                      choices = c('Standardized', 'Crude')),
          prettyRadioButtons("cid",
                             'Basic Cause by Chapter (ICD-10):',
                            choices = unique(obts_clean$ICD_chapter),
                             shape = "square",
                             status = 'warning',
                             bigger = T,
                             animation = "smooth")),
        
        mainPanel(width = 6,
          tabsetPanel(
            tabPanel("Map",
                     div(
                       id = "plot-container",
                       tags$img(src = "spinner.gif",
                                id = "loading-spinner"),
                       leafletOutput("map1", 
                                     width = "100%", 
                                     height = "900px"))),
            tabPanel("Data", 
                     tableOutput("table1")),
            tabPanel("README", 
                     includeMarkdown("README.md"))
          )
        )
    ))
  

# Define server logic 
server <- function(input, output) {

  obts1 <- reactive({
    obts_clean %>%
      filter(Year == input$year & ICD_chapter == input$cid)
  })
  
  pop_t1 <- reactive(
    pop_t_clean %>%
      filter(Year == input$year)
  )
  
  df1 <- reactive(
    right_join(obts1(), pop_t1(), by = c('Sex', 'agegroup', 'UF')) 
  )
  
  df2 <- reactive(
    left_join(df1(), standard_pop_clean, by = c('Sex', 'agegroup'))
  )
  
  # Calculating standarized rates based on R Epi handbook lesson
  mortality_ds_rate_phe <- 
    reactive(
      df2() %>%
        group_by(UF) %>%
        PHEindicatormethods::phe_dsr(
          x = Deaths,                
          n = pop.x,            
          stdpop = pop.y,              
          stdpoptype = "field") %>%
        mutate(Crude = total_count/total_pop * 100000)%>%
        rename(Standardized = value)
    )
  
  output$table1 <- renderTable({
    mortality_ds_rate_phe() %>%
      rename(Obitos = 'total_count',
             Populacao = 'total_pop') %>%
      select(c(UF,Obitos,Populacao,Crude,Standardized))
  })
  
  map <- reactive(
    left_join(UF_shp, mortality_ds_rate_phe(), by = c('NM_UF'= 'UF')) %>%
      mutate(inc_cat = cut(get(input$taxa), include.lowest = T,
                           breaks = c(quantile(get(input$taxa), by = 0.2, na.rm = T))))
    )
  
   pal <- reactive(
     colorFactor(
     palette = 'YlOrRd',
     domain = map()$inc_cat)
   )

   labels <- reactive(
     paste0("<strong>",map()$NM_UF,"</strong><br/>",
            "Standardized Rate: ", round(map()$Standardized,1),"<br/>",
            "Crude Rate: ", round(map()$Crude,1)
   ) %>% lapply(htmltools::HTML)
   )
   
   
  output$map1 <- renderLeaflet({
    
    # Thats out leaflet map
    leaflet(map()) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light")) %>%
      # set view
      setView(lng =-52.9500 ,lat = -10.6500, zoom = 4.5) %>%
      # Polygons
      addPolygons(
        fillColor = ~pal()(inc_cat),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      # Legend
       addLegend(values = map()$inc_cat,pal = pal(), opacity = 0.7, title = NULL,
                position = "topleft")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

