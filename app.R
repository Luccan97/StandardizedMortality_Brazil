#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(sf)
library(ggrepel)
library(bslib)
  

obts_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/obts_clean.csv", 
                       col_types = cols(Sex = col_character()), 
                       locale = locale(encoding = "ISO-8859-1"))


githubURL <- ("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/UF_shp.RDS")
download.file(githubURL,"UF_shp.rds", method="curl")
UF_shp <- readRDS("UF_shp.rds")


pop_t_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/pop_t_clean.csv")

standard_pop_clean <- read_csv("https://raw.githubusercontent.com/Luccan97/StandardizedMortality_Brazil/main/data/standard_pop_clean.csv")

# i18n <- Translator$new(translation_json_path='translations/translation.json')
# i18n$set_translation_language('en')

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = 'yeti'),
  
  # shiny.i18n::usei18n(i18n),
  # tags$div(
  #   style='float: right;',
  #   selectInput(
  #     inputId='selected_language',
  #     label=i18n$t('Change language'),
  #     choices = i18n$get_languages(),
  #     selected = i18n$get_key_translation()
  #   )),
  ## Stlysh CSS
  
  h1(id="tag1", "Open data <> Free knowledge <> Public Health."),
  
  h1(id="tag2", "Standardized mortality rate by sex and age group in the States of Brazil (2010-2019)."),

  tags$head(
    tags$style(HTML(
      '#tag1 {color: white;
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
    .tabbable > .nav > li > a[data-value="Mapa"]
      {background-color: #405d27;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
   
    }
    .tabbable > .nav > li > a[data-value="Dados"]
      {background-color: #405d27;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-size:20px;
    
      }
      .tabbable > .nav > li > a[data-value="README"]
      {background-color: white;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
    
      }
    
  '))),

  # Sidebar with brief description of dashboard utilities and input choices
    sidebarLayout(
      
        sidebarPanel(
          h4("What is it and how to use it?"),
          helpText("The dashboard offers an interactive visualization of the spatial distribution in the Federative Units of Brazil of the mortality rates standardized by sex and age group,
                   according to the basic causes grouped in the chapters of the ICD-10.
                   To learn more about the build process, click on the 'READme' tab of the panel.
                  
                   To create a specific map:
                   Select a year, rate type (standardized or gross), and the ICD-10 chapter of interest."),
          selectInput("year",
                      "Year od death:", 
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
        
        mainPanel(
          tabsetPanel(
            tabPanel("Map",
                    leafletOutput("map1", 
                                  width = "100%", 
                                  height = "900px")),
            tabPanel("Data", 
                     tableOutput("table1")),
            tabPanel("README", 
                     includeMarkdown("README.md"))
          )
        )
    ))
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # observeEvent(input$selected_language, {
  #   update_lang(session, input$selected_language)
  # })
  
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
            "Standardized Rate: ", round(map()$Padronizada,1),"<br/>",
            "Crude Rate: ", round(map()$Bruta,1)
   ) %>% lapply(htmltools::HTML)
   )
   
   
  output$map1 <- renderLeaflet({
    
    leaflet(map()) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light")) %>%
      setView(lng =-52.9500 ,lat = -10.6500, zoom = 5) %>%
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
       addLegend(values = map()$inc_cat,pal = pal(), opacity = 0.7, title = NULL,
                position = "topleft")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

