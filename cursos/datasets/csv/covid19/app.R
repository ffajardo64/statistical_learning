# File: data.R
# Version: 1.4
# Date: 2020-04-18
# Title: Dashboard Covid-19
# Author: Fabio Fajardo <fabio.molinares@ufes.br>
# Maintainer: Fabio Fajardo <fabio.molinares@ufes.br>
# Depends: R (>= 1.8.0), tidyverse, readxl, ggmap, rgdal, rgeos, 
#       maptools, dplyr, tidyr, tmap, sf , jsonlite, shinydashboard,
#       shiny, ggplot2, plotly, readCovid19
# ------------------------------------------------------------------
# ---------------------------            Install informations:
# https://leobastos.wordpress.com/2017/11/28/instalando-o-tmap/
# https://geocompr.github.io/post/2020/installing-r-spatial-ubuntu/
#
# Instalando o pacote sf em ubuntu
# https://philmikejones.me/tutorials/2018-08-29-install-sf-ubuntu/
# http://stackoverflow.com/questions/12141422/error-gdal-config-not-found 
# http://stackoverflow.com/questions/15248815/rgdal-package-installation
# sudo apt install -y libudunits2-0 libudunits2-dev
# sudo apt install libgdal-dev
# devtools::install_github("r-spatial/sf")
#
# ------------------------------------------------------------------
# Description: Dashboard Covid-19
# License: GPL (>= 2)
# URL: 
# Last update
# 18/04/2020: importando os dados da página https://covid.saude.gov.br
# 22/04/2020: download dos arquivos correspondentes ao SRAG do portal
#   do site Infogripe - http://info.gripe.fiocruz.br/
# 23/04/2020: implementação das funções downloadBR e downloadCovid19 
#   para baixar os os dados do COVID-19 do site: https://covid.saude.gov.br/
# 23/04/2020: 
# download.file("https://sidra.ibge.gov.br/geratabela?format=br.csv&name=tabela6579.csv&terr=N&rank=-&query=t/6579/n3/all/v/all/p/last%201/l/v,p,t&agruparNoCabecalho=false",method = "curl")
# 
#rm(list=ls(all=TRUE))
##
##
# Bibliotecas
library(abjutils)
library(curl)
library(data.table)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(DT)
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(httr)
library(jsonlite)
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
library(leafpop)
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.r-project.org")
library(rainbow)
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
library(rgdal)
library(rgeos) #https://statistique-et-logiciel-r.com/premiers-pas-en-cartographie-avec-r/
library(scales)
library(sf)
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(shinydashboardPlus)
library(stringr)
#library(tidyverse)
library(viridis)

# if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
# if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")


# ******************************************************************************
# *********************                                   Importando dados
# lendo dados do Ministério da Saúde - Brasil
# homepage: https://covid.saude.gov.br 
# Description: Painel Coronavírus
# ******************************************************************************
dados_covid19_brasil <- NULL
populacao_x_estado <-NULL

 dir<-"/home/ffajardo/Dropbox/working_papers/R_scripts/shinny_apps/covid19/"
 mapa_brasil <- readOGR(dsn = ".", layer = "BRUFE250GC_SIR",encoding = "UTF-8") #shapefile(file.choose())

 
 
# ------------------------------------------------------------------------------
# --------------------                            Estimativas populacionais
# ------------------------------------------------------------------------------
# Populacao por Estados
populacao_x_estado<-as.data.frame(read.csv("populacao.csv",row.names=NULL))
# ------------------------------------------------------------------------------
# --------------------                            Dados Covid-19
# ------------------------------------------------------------------------------
dados_covid19_brasil<-as.data.frame(read.csv("coronavirus.csv")) # repositório mantido pelo Ministério de saúde
dados_covid19_es<-as.data.frame(read.csv("covid19_es.csv"))
coordenadas_uf<-read.csv("coordenadas_geograficas_estados.csv",dec=",")

# Filtrados por regiao
data_ultimo_registro<-dados_covid19_brasil$data[length(dados_covid19_brasil$data)]

mapa_brasil_covid<-sf::st_read(dsn = "BRUFE250GC_SIR.shp", quiet = TRUE)%>%
  mutate(Cod_IBGE = as.numeric(as.character(CD_GEOCUF)))
# ------------------------------------------------------------------------------
# --------------------                            Dados SRAG
# ------------------------------------------------------------------------------
casos_srag_br<-as.data.frame(read.csv("casos_srag_br_2020s16.csv",sep=",",header = T, row.names=NULL))
casos_srag_uf<-as.data.frame(read.csv("casos_srag_uf_2020s16.csv",sep=",",header = T, row.names=NULL))
detalhes_srag_br<-as.data.frame(read.csv("detalhes_srag_br_2020s16.csv",sep=",",header = T, row.names=NULL))

# ------------------------------------------------------------------------------
# --------------------                            Dados Obitos SRAG
# ------------------------------------------------------------------------------
casos_obito_srag_br<-as.data.frame(read.csv("casos_obito_br_2020s16.csv",sep=",",header = T, row.names=NULL))
casos_obito_srag_uf<-as.data.frame(read.csv("casos_obito_uf_2020s16.csv",sep=",",header = T, row.names=NULL))
detalhes_obito_srag_br<-as.data.frame(read.csv("detalhes_srag_br_2020s16.csv",sep=",",header = T, row.names=NULL))

reds = colorNumeric("Oranges", domain = dados_covid19_brasil$obitosAcumulados[dados_covid19_brasil$data==data_ultimo_registro])
# set mapping colour for each outbreak
covid_col = "#045a8d" #"#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

basemap<- leaflet(mapa_brasil)%>%
  addTiles() %>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("COVID-19 (Acumulado)", "COVID-19 (diário)", "COVID-19 (óbitos)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("COVID-19 (diário)", "COVID-19 (óbitos)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  setView(lng = -55.019372, lat  = -25, zoom = 4) %>%
  addPolygons(weight = 0.7, fillColor = ~reds(dados_covid19_brasil$obitosAcumulados[dados_covid19_brasil$data==data_ultimo_registro]),
              color = "#820715",fillOpacity = 0.5,
              smoothFactor = 0.1)%>%
  addLegend("bottomright", pal = reds, values = dados_covid19_brasil$obitosAcumulados[dados_covid19_brasil$data==data_ultimo_registro],
            title = "<small>Número de óbitos</small>")

# ******************************************************************************
# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# ui <- dashboardPage(
#   
#   dashboardHeader(title="Dashboard Covid-19"),
#   
#   dashboardSidebar(sidebarMenu(
#     menuItem("Brasil", icon = icon("chart-bar"),
#              menuSubItem("Covid-19", tabName = "covidBR"),
#              menuSubItem("SRAG", tabName = "SARG_br")),
#     menuItem("Espírito Santo", icon = icon("chart-bar"),
#              menuSubItem("Covid-19", tabName = "covidES"),
#              menuSubItem("SRAG", tabName = "SARG_es"))
#   )),
#   
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "covidBR",
#               fluidRow(
#                 valueBoxOutput(width = 2, outputId = "value_populacao_br"), 
#                 valueBoxOutput(width = 2, outputId = "value_covid_confirmados_x_100k"), 
#                 valueBoxOutput(width = 2, outputId = "value_covid_confirmados"), 
#                 valueBoxOutput(width = 2, outputId = "value_covid_obitos"), 
#                 valueBoxOutput(width = 2, outputId = "value_srag_estimados"), 
#                 valueBoxOutput(width = 2, outputId = "value_obito_srag_estimados") 
#               ),
#               fluidRow(
#                 box(width = 12,title = "Informações por estado", status = "primary", solidHeader = T,
#                     column(width=4,selectInput(inputId = "select_regiao", label = "Região",
#                                choices = unique(regioes),
#                               selected = 1, width = "100%"
#                     )),
#                     column(width=4,selectInput(inputId ="select_estado", label ="UF",choices = NULL, width = "100%")
#                     ),
#                     column(width=4,selectInput(inputId ="select_info", label ="Casos",choices = NULL, width = "100%")
#                     )
#                 ),
#                 
#                # box(width = 6,
#                #    plotlyOutput(outputId = "hist_pop_regiao")),
#                box(width = 6,
#                    plotlyOutput(outputId="scatter_covid19BR")),
#                box(width = 6,
#                    leafletOutput(outputId="mapa_brasil_covid19", height = "500px")),
#                # box(width = 6,
#                #    plotlyOutput(outputId = "hist_petal_wid"))
#               )
#       ),
#       tabItem(tabName = "covidES",
#               fluidRow(
#                 valueBoxOutput(width = 2, outputId = "value_populacao_es"),
#                 valueBoxOutput(width = 2, outputId = "value_covid_confirmados_es_x_100k"), 
#                 valueBoxOutput(width = 2, outputId = "value_covid_confirmados_es"), 
#                 valueBoxOutput(width = 2, outputId = "value_covid_obitos_es"), 
#                 valueBoxOutput(width = 2, outputId = "value_srag_estimados_es"), 
#                 valueBoxOutput(width = 2, outputId = "value_obito_srag_estimados_es") 
#               ),
#               fluidRow(
#                 box(
#                   plotlyOutput(outputId="scatter_covid19_es"))
#                 #box(
#                 #  plotlyOutput(outputId = "hist_petal_len")),
#                 #box(
#                 #  plotlyOutput(outputId = "hist_petal_wid"))
#               )
#       )
#     ))
# )


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                               "Painel COVID-19", id="nav",
             tabPanel("Brasil",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 120, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h5("Dados COVID-19 Brasil")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        span(h4(textOutput("reactive_death_count"), align = "right"), style="color:#b30000"),
                                        span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        # plotOutput("epi_curve", height="130px", width="100%"),
                                        # plotOutput("cumulative_plot", height="130px", width="100%"),
                                        # 
                                        sliderInput("plot_date",
                                                    label = h5("Selecione a data"),
                                                    min = 1,#as.Date(min(dados_covid19_brasil$data)),
                                                    max = 10,#as.Date(data_ultimo_registro),
                                                    value = 10,#as.Date(data_ultimo_registro),
                                                    timeFormat = "%d %b",
                                                    animate=animationOptions(interval = 3000, loop = FALSE))
                          ),
                      )
             ), # fim tabPanel 1
             tabPanel("Espírito Santo")
  ) # Fim navbarPage
) # Fim ui function


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # output$scatter_covid19_es <- renderPlotly({
  #    data <- as.data.frame(dados_covid19_es%>%
  #      transmute(data=Data, casos=1, obitos=ifelse(Evolucao=="Óbito pelo COVID-19",1,0))%>%
  #      group_by(data)%>%
  #      summarise(casosA = sum(casos), obitosA=sum(obitos))%>%
  #      transmute(data,acumulados=cumsum(casosA),obitosAcumulados=cumsum(obitosA)))
  #    
  # plot <- plot_ly(data, 
  #                    type="scatter",
  #                    x=~data, 
  #                    y=~acumulados,
  #                    name = 'Confirmados',
  #                    mode = "lines",
  #                    line = list(
  #                           color = '#0d8257'
  #                    ))%>%
  #          add_trace(
  #              type = "scatter",
  #              x = ~data,
  #              y = ~obitosAcumulados,
  #              name = 'Óbitos',
  #              mode = "lines",
  #              line = list(color = '#b30b0b'))%>%
  #          layout(
  #              title = "Dinâmica do Covid-19 no Espírito Santo",
  #              xaxis = list(title="Data",
  #                           type = 'date',
  #                           tickformat = "%d/%m/%Y"
  #               ),
  #              yaxis = list(
  #                title = "Casos"
  #              )
  #           )
  #    ggplotly(plot)
  #  })
  #   
  #   output$value_srag_estimados_es <- renderValueBox({
  #     valueBox(
  #       value = prettyNum(sum(casos_srag_uf[casos_srag_uf$ano=="2020" & casos_srag_uf$territory_name=="Espírito Santo",3]),big.mark = ".",decimal.mark = ","), 
  #       subtitle = tagList(
  #         tags$p("Estimados", style = "font-size: 80%;"),
  #         tags$p("SRAG - Semana 16", style = "font-size: 150%;")
  #       ),
  #       icon = icon("user-check"),
  #       color = "maroon"
  #     )
  #   })
  #   
  #   output$value_obito_srag_estimados_es <- renderValueBox({
  #     valueBox(
  #       value = prettyNum(sum(casos_obito_srag_uf[casos_obito_srag_uf$ano=="2020" & casos_obito_srag_uf$territory_name=="Espírito Santo", 3]),big.mark = ".",decimal.mark = ","),
  #       subtitle = tagList(
  #         tags$p("Óbitos Estimados", style = "font-size: 80%;"),
  #         tags$p("SRAG - Semana 16", style = "font-size: 150%;")
  #       ),
  #       icon = icon("cross"),
  #       color = "maroon"
  #     )
  #   })
    
    
    reactive_dados_covid_uf <- reactive({
      dados_covid19_brasil %>%
        filter(data==data_ultimo_registro)%>%
        mutate(populacao=populacao_x_estado$populacao, UF=populacao_x_estado$UF,confirmados_x_100k_hab=round(casosAcumulados/populacao*100000,digits=2),obitos_x_100k_hab=round(obitosAcumulados/populacao*100000,digits=2),fatalidade=round(obitosAcumulados/casosAcumulados*100,digits=2))
    })

    
    reactive_dados_uf <- reactive({
      reactive_dados_covid_uf() %>%
        transmute(regiao,UF=estado, casosNovos, casosAcumulados, obitosNovos, obitosAcumulados,confirmados_x_100k_hab,obitos_x_100k_hab,fatalidade)%>%
        right_join(coordenadas_uf, by = "UF")%>%
        right_join(populacao_x_estado, by = "UF")%>%
        transmute(regiao,UF,casosNovos,casosAcumulados,obitosNovos,obitosAcumulados,confirmados_x_100k_hab,obitos_x_100k_hab,fatalidade,Estado,Cod_IBGE,Capital,lat,lon,populacao)
    })
    
   
    reactive_joined_dados_covid<- reactive({
      mapa_brasil_covid%>%
        left_join(reactive_dados_uf(), by = "Cod_IBGE")
    })
    
        
    output$reactive_case_count <- renderText({
      paste0(prettyNum(sum(reactive_dados_covid_uf()$casosAcumulados),big.mark = ".",decimal.mark = ","), " Casos")
    })
    
    output$reactive_death_count <- renderText({
      paste0(prettyNum(sum(reactive_dados_covid_uf()$obitosAcumulados),big.mark = ".",decimal.mark = ","), " Óbitos")
    })
    
    output$reactive_recovered_count <- renderText({
      paste0(formatC(sum(reactive_dados_covid_uf()$casosAcumulados)/sum(populacao_x_estado$populacao)*100000,digits = 2, decimal.mark = ","), " Confirmados x 100k hab.")
    })
    
    output$reactive_active_count <- renderText({
      paste0(formatC(sum(reactive_dados_covid_uf()$obitosAcumulados)/sum(reactive_dados_covid_uf()$casosAcumulados)*100, digits=2,decimal.mark=","), "% Taxa de fatalidade")
    })
    
    
    
    output$mymap <- renderLeaflet({
     basemap 
    })
    
    
    observeEvent(input$plot_date, {
      leafletProxy("mymap") %>% 
        clearMarkers() %>%
        #clearShapes() %>%
         addPolygons(data = mapa_brasil,
                     stroke = FALSE, 
                     smoothFactor = 0.1,
                     fillOpacity = 0.5,
                     fillColor = ~reds(reactive_joined_dados_covid()$obitosAcumulados)) %>%
        
        addCircleMarkers(data = reactive_joined_dados_covid(), lat = ~lat, lng = ~lon, weight = 1, radius = ~(casosAcumulados)^(1/4), 
                         fillOpacity = 0.1, color = covid_col, group = "COVID-19 (Acumulado)",
                         label = sprintf("<strong>%s - %s Hab.</strong><br/>Total de casos: %s<br/>Total de óbitos: %s<br/><br/><strong>Casos por 100k hab.</strong><br/>Confirmados: %s<br/>Óbitos: %s<br/><br/><strong>Taxa de fatalidade por casos</strong>: %s%%",reactive_joined_dados_covid()$Estado,prettyNum(reactive_joined_dados_covid()$populacao,big.mark = ".",decimal.mark = ","),prettyNum(reactive_joined_dados_covid()$casosAcumulados,big.mark = ".",decimal.mark = ","),prettyNum(reactive_joined_dados_covid()$obitosAcumulados,big.mark = ".",decimal.mark = ","),prettyNum(reactive_joined_dados_covid()$confirmados_x_100k_hab,big.mark = ".",decimal.mark = ","),prettyNum(reactive_joined_dados_covid()$obitos_x_100k_hab,big.mark = ".",decimal.mark = ","),prettyNum(reactive_joined_dados_covid()$fatalidade,big.mark = ".",decimal.mark = ",")) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                           textsize = "15px", direction = "auto")) #%>%
        
        # addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
        #                  fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
        #                  label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k, reactive_db()$deathsper100k) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
        #                    textsize = "15px", direction = "auto")) %>%
        # 
        # addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
        #                  fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
        #                  label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
        #                    textsize = "15px", direction = "auto"))  %>%
        # 
        # addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
        #                  fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
        #                  label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
        #                    textsize = "15px", direction = "auto")) %>%
        # 
        # addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
        #                  fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
        #                  label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
        #                    textsize = "15px", direction = "auto")) %>%
        # 
        # addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
        #                  fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
        #                  label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
        #                    textsize = "15px", direction = "auto"))
    })
    
}


# Run the application shiny::runApp(display.mode="showcase")
shinyApp(ui = ui, server = server)
