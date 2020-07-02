#LIBRARY####
library(shinydashboard)
library(shiny)
library(plotly)
library(dplyr)
library(forecast)
library(tableHTML)
library(lubridate)
library(quantmod)
library(rugarch)
library(tseries)
library(ggplot2)
library(TSstudio)
library(shinyjs)
library(shinycssloaders)
library(PerformanceAnalytics)
library(rmgarch)
library(shinycustomloader)

marquee_list <- list(marquee("Please,be patient ",style="font-size:40px"))

options(spinner.color="#23395d", spinner.color.background="#ffffff", spinner.size=2)


source('garchAuto.R')

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Univariate Models", icon = icon("hdd",lib='font-awesome'), tabName = "widgets"),
    menuItem("Multivariate Models", icon = icon("wallet",lib='font-awesome'), tabName = "wallet"),
    menuItem("Author's Page", icon = icon("file-code-o"), href = "https://github.com/CarmineMinichini")
  )
)

body <- dashboardBody( useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customize.css"))
  ,  
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
            box(title=span(icon("donate",lib='font-awesome'), "Select Ticker"), width=12,
            textInput('datestart','Type start date',value='21/04/16',placeholder = '21/04/16'),
            textInput('dateend','Type end date',value='21/04/20',placeholder = '21/04/20'),
            textInput('ticker','Type ticker',value='AAPL',placeholder = 'AAPL')
            ),
            box(title='Close Prices',width=6,
                withLoader(plotlyOutput("plot0",height = 350,width = 600)),solidHeader = TRUE
            ),
            box(title='Returns ',width=6,
              withLoader(plotlyOutput("plot1",height = 350,width = 600)),solidHeader = TRUE
            )
            )#fluidRow
    ),
    tabItem(tabName = "widgets",
            fluidRow(
            box(title=span(icon("bullseye",lib='font-awesome'), "Select Garch Parameters"),width=12,
            selectInput('garchtype','Select Garch:',c('sGARCH','eGARCH','gjrGARCH','iGARCH','csGARCH'),selected='sGARCH'),
            selectInput('dist','Select Distribution:',c('norm','snorm','std','sstd','ged','sged','nig'),selected='norm'),
            textInput('testsize','Select test size( in days)',value='500',placeholder = '500'),
            sliderInput('armap','Select Arma P order',min=0,max=10,value=0),
            sliderInput('armaq','Select Arma Q order',min=0,max=10,value=0)
            ),
            box(title='Suggested Parameters for Model',width=12,
                withLoader(verbatimTextOutput('garchAuto'),type='text',loader = marquee_list) ,collapsible = TRUE,
            ),
            box(width=4,
                withLoader(plotOutput("plot2",height = 350,width = 400),type='text',loader = marquee_list),status = 'success',solidHeader = TRUE
            ),
            box(width=4,
                withLoader(plotOutput("plot3",height = 350,width = 400),type='text',loader = marquee_list) ,status = 'success',solidHeader = TRUE
            ),
            box(width=4,
                withLoader(plotOutput("plot4",height = 350,width = 400),type='text',loader = marquee_list) ,status = 'success',solidHeader = TRUE
            ),
            box(title='Series Forecast',width=6,
                withLoader(plotOutput("plot5",height = 350,width = 600),type='text',loader = marquee_list),solidHeader = TRUE
            ),
            box(title='Volatility Forecast',width=6,
                withLoader(plotOutput("plot6",height = 350,width = 600),type='text',loader = marquee_list),solidHeader = TRUE
            ),
            box(title='Backtesting Report',width=12,
                withLoader(verbatimTextOutput('backtest'),type='text',loader = marquee_list) ,collapsible = TRUE,
            ),

            )#FluidRow
            ),#TabItem2
    tabItem(tabName = "wallet",
            fluidRow(
              box(title=span(icon("donate",lib='font-awesome'), "Build Wallet"), width=6,
                  textInput('datestart1','Type start date',value='21/04/16',placeholder = '21/04/16'),
                  textInput('dateend1','Type end date',value='21/04/20',placeholder = '21/04/20'),
                  textInput('ticker1','Type 1st ticker',value='SPY',placeholder = 'SPY'),
                  textInput('ticker2','Type 2nd ticker',value='QQQ',placeholder = 'QQQ'),
                  textInput('quota1','Type shares 1st ticker',value=10 ,placeholder =10),
                  textInput('quota2','Type shares  2nd ticker',value=20,placeholder = 20 )
                  ),
              box(title=span(icon("bullseye",lib='font-awesome'), "Select Multivariate Parameters"),width=6,
                  selectInput('mvdist','Choose Multivariate distribution',c('mvnorm','mvt'),selected='mvnorm'),
                  textInput('testsizedcc','Select test size(in days) for DCC',value='500',placeholder = '500')
              ),
              box(title='Returns PF',width=12,
                  withLoader(plotlyOutput("plot_wallet",height = 350,width = 1350)),solidHeader = TRUE
              ),
              infoBox(title='Multivariate Models',subtitle ='Modelled Correlation take specification from Univariate Menu',
                      width=12,icon=icon('hdd',lib='font-awesome'),
                      fill = TRUE,color = 'navy'),
              box(title='DCC Correlation',width=6,
                  withLoader(plotOutput("plot_dcc",height = 350,width = 650)),solidHeader = TRUE
              ),
              box(title='DCC Forecast',width=6,
                  withLoader(plotOutput("plot_dcc_forecast",height = 350,width = 650)),solidHeader = TRUE
              ),
              )
            
    )#TabItem3
  )#TabItems
)# dashboard Body

# Put them together into a dashboardPage
ui= dashboardPage(skin='black',title='Garch Explorer',
  dashboardHeader(title = span(icon("money-bill-wave",lib='font-awesome'), "GarchExp")),
  sidebar,
  body
)
