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
library(plotly)
library(ggrepel)
library(shiny.i18n)

load("C:/projetos/BrazilMortalityRate/data.RData")

# i18n <- Translator$new(translation_json_path='translations/translation.json')
# i18n$set_translation_language('en')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
  
  h1(id="tag1", "Dados abertos <> Conhecimento livre <> Saúde Pública."),
  
  h1(id="tag2", "Taxa de mortalidade padronizada por sexo e faixa etária nos Estados do Brasil (2010-2019)."),
  
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
                  font-family:"Monaco",Georgia,Serif;
                  }
    #tag2 {color: #405d27; 
                  background-color: white;
                  border-radius: 15px 50px 30px;
                  border:2px solid #c1946a;
                  font-size: 25px;
                  font-style: bold;
                  text-align:center;
                  padding-top:10px;
                  padding-bottom:10px;
                  font-family:"Monaco",Georgia,Serif;
    }
    .tabbable > .nav > li > a[data-value="Mapa"] 
      {background-color: #405d27; 
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
    font-family:"Monaco",Georgia,Serif;
    }
    .tabbable > .nav > li > a[data-value="Dados"] 
      {background-color: #405d27;
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-size:20px;
    font-family:"Monaco",Georgia,Serif;
      }
      .tabbable > .nav > li > a[data-value="README"] 
      {background-color: white; 
      border:2px solid #c1946a;
    color:#c1946a;
    width: 8vw;
    text-align:center;
    font-style:bold;
    font-size:20px;
    font-family:"Monaco",Georgia,Serif;
    }
  '))),
  
  # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          h4("O que é e como utilizar?"),
          style = '
          border-radius: 15px 50px 30px;
          border:2px solid #405d27;
          font-family:"Monaco",Georgia,Serif;
          padding-top:30px;
          padding-bottom:10px;
          font-size: 15px;',

          helpText("O dashboard oferece uma vizualização interativa da distribuição espacial nas Unidades Federativas do Brasil das taxas de mortalidade padronizadas por sexo e feixa etária
                   de acordo com as causas básicas agrupadas nos capítulos do CID-10. 
                   Para saber mais a respeito do processo de construção, clique na aba 'READme' do painel.
                  
                   Para criar um mapa específico:
                   Selecione um ano, o tipo de taxa (padronizada ou bruta) e o capítulo do CID-10 de interesse."),
          selectInput("year",
                      "Ano:", selected = "2019",
                      choices = c('2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')),
          selectInput("taxa",
                      "Taxa:",
                      choices = c('Padronizada', 'Bruta')),
          prettyRadioButtons("cid",
                             'Causa básica por Capítulo (CID-10):',
                            choices = unique(obts_clean$ICD_chapter),
                             shape = "square",
                             status = 'warning',
                             bigger = T,
                             animation = "smooth")),
        
        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(
            tabPanel("Mapa",
                     plotOutput("map1", width = "100%", height = "900px")),
            tabPanel("Dados", tableOutput("table1")),
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
          x = Deaths,                 # column with observed number of events
          n = pop.x,             # column with non-standard pops for each stratum
          stdpop = pop.y,               # standard populations for each stratum
          stdpoptype = "field") %>%
        mutate(Bruta = total_count/total_pop * 100000)%>%
        rename(Padronizada = value)
    )
  
  output$table1 <- renderTable({
    mortality_ds_rate_phe()
  })
  
  map <- reactive(
    left_join(UF_shp, mortality_ds_rate_phe(), by = c('NM_UF'= 'UF')) %>%
      mutate(inc_cat = cut(get(input$taxa), include.lowest = T,
                           breaks = c(quantile(get(input$taxa), by = 0.2, na.rm = T))))
    )
  
 
 
  ## Mapa
  
  # Paleta de cores da gradação de taxas de mortaldiade
  
  pal <- hcl.colors(5, "Heat", rev = TRUE, alpha = 0.7)
  
  output$map1 <- renderPlot({
    
    ggplot(map()) +
      geom_sf(aes(fill = inc_cat),
              color = "black",
              lwd = 0.35) + 
      ggtitle(paste0("Taxa de mortalidade ",input$taxa ," por ", input$cid, " em ", input$year))+
      labs(fill = "", x="", y ="")+
      scale_fill_manual(values = pal,
                        drop = FALSE,
                        na.value = "#c1946a")+
      geom_sf_label(aes(label = round(get(input$taxa),1)), colour = "black", na.rm = T, size = 4)+
      coord_sf()+
      theme_bw()+
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            legend.position = "left",
            legend.direction = "vertical",
            legend.text = element_text(size = 15),
            panel.border = element_rect(colour = "#405d27", fill=NA, size=2),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

