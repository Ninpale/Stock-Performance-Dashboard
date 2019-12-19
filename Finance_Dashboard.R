#Deploy
rsconnect::setAccountInfo(name='ninpale',
                          token='B9EAB9ECEC77D71151D2306495F9D063',
                          secret='EWHwGNzVdxWSer1PMg3aDPbphj0DY6/BEECwRFgJ')


# WELCOME INSIDE MY DASHBOARD


# Library

  #R
library(htmlwidgets)
library(tidyverse)
library(plotly)
library(gganimate)
library(ggrepel)
library(cowplot)
library(pipeR)
library(dplyr)
library(gapminder)
library(RColorBrewer)
library(lubridate)
library(viridis)

library(rsconnect)

  #R_Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)

#Loading Dataset
load("Full_dataset.RData")


#APP
ui <- dashboardPage(skin = "black",
                    
  dashboardHeader(title = strong("FINANCE ANALYSIS")),
  dashboardSidebar( 
    column(12, h2("CONTROL")),
    fluidPage(
      chooseSliderSkin("Modern", color = "black")
      ),
    sliderInput("slider_time", h4("DATES"),
                min = as.Date("31/12/2007","%d/%m/%Y"),
                max = as.Date("27/09/2019","%d/%m/%Y"),
                value = c(as.Date("31/12/2007", "%d/%m/%Y"),as.Date("27/09/2019", "%d/%m/%Y")), step = 0.25
    ),
    checkboxGroupInput("Sector", h4("SECTORS"),
                       choices = list("Technology" = "Technology", 
                                      "Consumer Cyclical" = "Consumer Cyclical",
                                      "Financial Services" = "Financial Services",
                                      "Communication Services" = "Communication Services"),
                       selected = c("Technology", "Consumer Cyclical", "Financial Services", "Communication Services")
    ),
    checkboxGroupInput("Country", h4("COUNTRIES"),
                       choices = list("France" = "France", 
                                      "Germany" = "Germany",
                                      "Spain" = "Spain",
                                      "Italy" = "Italy",
                                      "United Kingdom" = "United Kingdom",
                                      "United-States" = "United-States"),
                       selected = c("United-States", "United Kingdom", "Italy", "Spain", "Germany", "France")
                       
    ),
    tags$div(class="header", checked=NA,
             tags$a(href="https://www.linkedin.com/in/adriencoucaud/", 
                    h1(icon("linkedin", lib = "font-awesome")), 
                    target = "_blank"
                    ),
             tags$a(href="mailto:adriencoucaud@gmail.com?subject=I loved your dashboard, let's get in touch!", 
                    h1(icon("envelope-square", lib = "font-awesome"))
                    )
             )
    ),
  
  dashboardBody(
    tags$head( 
      tags$style(HTML(".fa { font-size: 38px; }"))
    ),
    tags$style(".fa-thumbs-down {color:#FFAEA0}"),
    tags$style(".fa-thumbs-up {color:#B0FFB1}"),
    tags$style(".fa-percent {color:#FFFFFF}"),
    tags$style(".fa-linkedin {color:#FFFFFF}"),
    tags$style(".fa-envelope-square {color:#FFFFFF}"),
    tabBox( height = "720px", width = 12,
            tabPanel(
              "General", 
              fluidRow(
                valueBox(
                  textOutput("nb_stocks"), "stocks are analyzed", color = "black"),
                valueBox(
                  textOutput("best_stock"), "performed the best during this period", color = "black",
                  icon("thumbs-up", lib = "font-awesome"),
                  ),
                valueBox(
                  textOutput("worse_stock"), "performed the worst during this period", color = "black",
                  icon("thumbs-down", lib = "font-awesome"))
              ),
              fluidRow(
                box(
                  plotOutput("Stocks"), width = 4
                ),
                box(
                  plotOutput("Indus"), width = 8
                )
              ),
              fluidRow(
                box(
                  plotOutput("country_vol"), width = 6
                ),
                box(
                  plotOutput("sector_vol"), width = 6
                )
              )
            ),
            tabPanel(
              "Focus on best stock",
              fluidRow(
                valueBox(
                  textOutput("target"), "is analyzed", color = "black"),
                valueBox(
                  textOutput("maxR"), "is the best daily return", color = "black",
                  icon("percent", lib = "font-awesome")),
                valueBox(
                  textOutput("minR"), "is the worst daily return", color = "black",
                  icon("percent", lib = "font-awesome"))
              ),
              fluidRow(
                box(
                  plotOutput("trend"), width = 6
                ),
                box(
                  plotOutput("point"), width = 6
                )
              ),
              fluidRow(
                box(
                  plotOutput("distrib"), width = 12
                )
              )
            )
          )
        )
)

server <- function(input, output){
  
#Font
  font_futura <- list(
    family = "Futura",
    size = 9,
    color = 'black') #Didn't work out
  
#Reactive filters  
  data <- reactive({
    Full_dataset <- Full_dataset %>%
      filter(Date >= as.Date(input$slider_time[1], "%d/%m/%Y") & Date <= as.Date(input$slider_time[2], "%d/%m/%Y"),
             SECTOR %in% input$Sector,
             COUNTRY %in% input$Country) 
    return(Full_dataset)
  })  
  
  dataVec <- reactive({
    Full_dataset <- Full_dataset %>%
      filter(Date %in% c(as.Date(input$slider_time[1], "%d/%m/%Y"), as.Date(input$slider_time[2], "%d/%m/%Y")),
             SECTOR %in% input$Sector,
             COUNTRY %in% input$Country) 
    return(Full_dataset)
  })
  
  dataFoc <- reactive({
    Full_dataset <- Full_dataset %>%
      filter(Date >= as.Date(input$slider_time[1], "%d/%m/%Y") & Date <= as.Date(input$slider_time[2], "%d/%m/%Y"),
             COMPANY.NAME == data_best_stock()$COMPANY.NAME,
             SECTOR %in% input$Sector,
             COUNTRY %in% input$Country)
    return(Full_dataset)
  })
  
  
# TAB 1 - General
  # Value boxes
    # Count of Companies in the calculation
  data_nb_stocks <- reactive(
    tmp <- data() %>%
      count(COMPANY.NAME)
  )
  
  output$nb_stocks <- renderText(nrow(data_nb_stocks()))
  
  
    # Best performing stock
  data_best_stock <- reactive(
    tmp <- data() %>%
      group_by(COMPANY.NAME) %>%
      summarise(return = prod(1+Return, na.rm=TRUE)-1) %>%
      arrange(desc(return)) %>%
      head(1)
  )
  
  output$best_stock <- renderText(data_best_stock()$COMPANY.NAME)
      
  
    # Worst performing stock
  data_worse_stock <- reactive(
    tmp <- data() %>%
      group_by(COMPANY.NAME) %>%
      summarise(return = prod(1+Return, na.rm=TRUE)-1) %>%
      arrange(return) %>%
      head(1)
  )
  
  output$worse_stock <- renderText(data_worse_stock()$COMPANY.NAME)
  
  
  
  # Graphs
    #TOP 5 INDUSTRIES
  data_topIndus <- reactive(
    tmp <- data() %>%
      count(INDUSTRY) %>%
      mutate(perc = n / nrow(data())) %>%
      arrange(desc(perc))
    )

  output$Indus <- renderPlot(                             
    ggplot(head(data_topIndus(), 5), aes(x = reorder(INDUSTRY, -perc), y = perc, fill = perc)) + 
      ggtitle("MOST REPRESENTED INDUSTRIES") +
      geom_col() +
      labs(y = "PERCENT", x="INDUSTRIES") +
      theme(axis.text.x=element_text(angle=90)) +
      coord_flip() +
      theme_light() +
      scale_fill_viridis(option="magma") +
      scale_y_continuous(labels = scales::percent) 
  )
  
  
    #TOP 5 STOCKS
  data_topStocks <- reactive (
    tmp <- data() %>%
      group_by(COMPANY.NAME) %>%
      summarise(return = prod(1+Return, na.rm=TRUE)-1) %>%
      arrange(desc(return))
  )
                                
  output$Stocks <- renderPlot(                             
    ggplot(head(data_topStocks(),5), aes(x = reorder(COMPANY.NAME, -return), y = return, fill = return)) +
      geom_col() +
      ggtitle("BEST 5 PERFORMING STOCKS") +
      labs(y = "RETURN", x="COMPANY") +
      coord_polar() +
      theme_light() +
      scale_fill_viridis(option="magma")
  )
  
    #Volatility per country
  data_country_vol <- reactive(
    tmp <- data() %>%
      group_by(SECTOR) %>%
      summarise(volatility = sd(Return, na.rm=TRUE))
  )
  
  output$country_vol <- renderPlot(
    ggplot(data_country_vol(), aes(x = reorder(SECTOR, -volatility), y = volatility, fill = volatility)) +
      geom_col() +
      ggtitle("VOLATILITY PER SECTOR") +
      labs(y = "VOLATILITY", x="SECTORS") +
      theme_light() +
      scale_fill_viridis(option="magma") +
      scale_y_continuous(labels = scales::percent)
  )
  
    #Volatility per sector
  data_sector_vol <- reactive(
    tmp <- data() %>%
      group_by(STOCK.EXCHANGE) %>%
      summarise(volatility = sd(Return, na.rm=TRUE))
  )
  
  output$sector_vol <- renderPlot(
    ggplot(data_sector_vol(), aes(x = reorder(STOCK.EXCHANGE, -volatility), y = volatility, fill = volatility)) +
      geom_col() +
      ggtitle("VOLATILITY PER STOCK EXCHANGE") +
      labs(y = "VOLATILITY", x="STOCK EXCHANGES") +
      theme_light() +
      scale_fill_viridis(option="magma") +
      scale_y_continuous(labels = scales::percent)
  )
  
# TAB 2 - Focus on best stock
  #ValueBox
    #Target
  data_target <- reactive(
    tmp <- data() %>%
      group_by(COMPANY.NAME) %>%
      summarise(return = prod(1+Return, na.rm=TRUE)-1) %>%
      arrange(desc(return)) %>%
      head(1)
  )
  
  output$target <- renderText(data_target()$COMPANY.NAME)  
  
    #Max Return
  data_maxR <- reactive(
    tmp <- dataFoc() %>%
      group_by(COMPANY.NAME) %>%
      summarise(max_return = round(max(Return)*100, 2))
  )
  
  output$maxR <- renderText(data_maxR()$max_return)
  
    #Min Return
  data_minR <- reactive(
    tmp <- dataFoc() %>%
      group_by(COMPANY.NAME) %>%
      summarise(min_return = round(min(Return)*100, 2))
  )
  
  output$minR <- renderText(data_minR()$min_return)
  
  
  #Graphs
    #Returns distribution
  data_distrib <- reactive(
    tmp <- dataFoc()
  )
  
  output$distrib <- renderPlot(
    ggplot(data_distrib(), aes(x=Return)) +
      ggtitle("DAILY RETURN DISTRIBUTION") +
      labs(y = "DENSITY", x="RETURN") +
      theme_light() +
      geom_density(fill = "#FFF0CC")
  )
  
    #Performance Trend
  data_trend <- reactive(
    tmp <- dataFoc()
  )
  
  output$trend <- renderPlot(
    ggplot(data_trend(), aes(x = Date, y = Adj.Close)) +
      geom_point(aes(x= Date, y = Adj.Close), size = 0.03) +
      ggtitle("STOCK PRICE EVOLUTION") +
      labs(y = "STOCK PRICE", x="TIME") +
      theme_light() +
      geom_smooth(color = "#FFF0CC")
  )

    #Return over Volatility scatterplot
  data_point <- reactive(
    tmp <- data() %>%
      group_by(COMPANY.NAME) %>%
      summarise(volatility = sd(Return, na.rm=TRUE),
                return = prod(1+Return, na.rm=TRUE)-1) %>%
      filter(return > -5)
  )
  
  output$point <- renderPlot(
    ggplot(data_point(), aes(x=volatility, y=return)) +
      ggtitle("RETURN OVER VOLATILITY") +
      labs(x = "VOLATILITY", y = "RETURN") +
      geom_point(color = "#FFF0CC", size = 3) +
      geom_point(data = data_point() %>% filter(COMPANY.NAME == data_best_stock()$COMPANY.NAME),
                 color = 'black', size = 10) +
      geom_label_repel(aes(label=ifelse(COMPANY.NAME == data_best_stock()$COMPANY.NAME,as.character(COMPANY.NAME),'')),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'black') +
      theme_light() +
      scale_y_continuous(labels = scales::percent)
  )
}

shinyApp(ui = ui, server = server)