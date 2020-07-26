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
              tags$style(make_css(list('.box', 
                                       c('font-size', 'font-family', 'color'), 
                                       c('13px', 'Courier', '#1e6a90')))),
              h4(strong("β"), align="left", style = "font-family: 'Lobster',cursive;
    font-weight: 2; font-size: 100px; line-height:0; color: #1e6a90;"),
            box(title=span(icon("donate",lib='font-awesome'), "Select Ticker"), width=6,
                helpText('By default dates range is of 1000 days(4 years) from current date'),
                helpText("Please note that univariate models needs at least 1000 observation to be sure of a good convergence"),
                tags$style("#ticker {background-color:#23395d;}"),
            dateRangeInput('daterange','Select Date range:',start=Sys.Date()-1451,end=Sys.Date(),startview = 'month'),
            helpText('Please Note that not every ticker are uptated until current date'),
            helpText(a(href='https://it.finance.yahoo.com/','Search on YAHOO Finance ')),
            textInput('ticker','Type ticker',value='AAPL',placeholder = 'AAPL'),
            ),
            tags$script(HTML("$('.box').eq(0).css('border', '5px solid #1e6a90');")),
            box(title='Close Prices',width=6,
                withLoader(plotlyOutput("plot0",height = 350,width = 650)),solidHeader = TRUE,
            ),
            box(title='Returns ',width=4,
              withLoader(plotlyOutput("plot1",height = 350,width = 400)),solidHeader = TRUE
            ),
            box(title='Autocorrelation',width=4,
                withLoader(plotlyOutput("plotacf",height = 350,width = 400)),
                solidHeader = TRUE
            ),
            box(title='Partial Autocorrelation',width=4,
                withLoader(plotlyOutput("plotpacf",height = 350,width = 400)),
                solidHeader = TRUE
            ),
            valueBox('Univariate Model','GARCH',icon=icon('bullseye',lib='font-awesome'),color='navy',width = 12),
            #################################################################################### UNIVARIATE MODEL
            box(title=span(icon("bullseye",lib='font-awesome'), "Select Garch Parameters"),width=12,
                selectInput('garchtype','Select Garch:',c('sGARCH','eGARCH','gjrGARCH','iGARCH','csGARCH'),selected='sGARCH'),
                selectInput('dist','Select Distribution:',c('norm','snorm','std','sstd','ged','sged','nig'),selected='norm'),
                helpText('Minimizing the AIC, we suggest you the best order for ARMA(p,q).'),
                helpText('Please note that modifying ARMA order you\'re modifying also volatility estimation.'),
                withLoader(verbatimTextOutput('garchAuto'),type='text',loader = marquee_list) ,collapsible = TRUE,
                sliderInput('armap','Select Arma(p,.)',min=0,max=10,value=0),
                sliderInput('armaq','Select Arma(.,q)',min=0,max=10,value=0),
            ),
            box(width=4,
                withLoader(plotOutput("plot2",height = 350,width = 400),type='html',loader = 'loader9'),status = 'success',solidHeader = TRUE
            ),
            box(width=4,
                withLoader(plotOutput("plot3",height = 350,width = 400),type='html',loader = 'loader9') ,status = 'success',solidHeader = TRUE
            ),
            box(width=4,
                withLoader(plotOutput("plot4",height = 350,width = 400),type='html',loader = 'loader9') ,status = 'success',solidHeader = TRUE
            ),
            valueBox('GARCH','Rolling Forecast on test size',color = 'navy',width=12),
            box(width=12,
                helpText('Please note that, if you choose an ARMA(0,0), there will not be any forecast for returns,for this purpose
                         we remind you to choose an appropriate ARMA(p,q) order, as suggested before'),
                helpText('Choose how many days in the past you want to try model ability:'),
              textInput('testsize','Select test size(in days)',value='100',placeholder = '100')
                ),
            box(title='Returns Forecast',width=6,
                withLoader(plotOutput("plot5",height = 350,width = 600),type='html',loader = 'loader9'),solidHeader = TRUE
            ),
            box(title='Volatility Forecast',width=6,
                withLoader(plotOutput("plot6",height = 350,width = 600),type='html',loader = 'loader9'),solidHeader = TRUE
            ),
            tabBox(title=span(icon("chart-line",lib='font-awesome'), "Risk Measure"),width=12,
                   tabPanel('VaR Backtesting Report',verbatimTextOutput('backtest')),
                   tabPanel('VaR',plotOutput('varplot',height=600,width=1350))
            ),
            valueBox(' GARCH Forecast','10 step ahead Forecast',width = 12,color='navy'),
            box(width=6,
                withLoader(plotOutput("plotfrcst",height = 350,width = 600),type='html',loader = 'loader9'),solidHeader = TRUE
                ),
            box(width=6,
                withLoader(plotOutput("plotfrcstvol",height = 350,width = 600),type='html',loader = 'loader9'),solidHeader = TRUE
            ),
            tabBox(title=span(icon("balance-scale",lib='font-awesome'), "Results"),width=12,
                   tabPanel('Garch Forecast',verbatimTextOutput('garchforecast')),
                   tabPanel('Garch Estimation',verbatimTextOutput('garchestim'))
                   )
              )#fluidRow1
    ),#TabItem1
    tabItem(tabName = "wallet",
            fluidRow(
              box(title=span(icon("donate",lib='font-awesome'), "Build Wallet"), width=6,
                  dateRangeInput('rangemulti','Select Date range:',start=Sys.Date()-1451,end=Sys.Date(),startview = 'month'),
                  textInput('ticker1','Type 1st ticker',value='SPY',placeholder = 'SPY'),
                  textInput('ticker2','Type 2nd ticker',value='QQQ',placeholder = 'QQQ'),
                  ),
              box(title=span(icon("bullseye",lib='font-awesome'), "Select Multivariate Parameters"),width=6,
                  ############## GARCH
                  helpText('Choose Garch Specification for each series'),
                  selectInput('garchtypemulti','Select Garch:',c('sGARCH','eGARCH','gjrGARCH','iGARCH','csGARCH'),selected='sGARCH'),
                  selectInput('distmulti','Select Distribution:',c('norm','snorm','std','sstd','ged','sged','nig'),selected='norm'),
                  sliderInput('armapmulti','Select Arma(p,.)',min=0,max=10,value=0),
                  sliderInput('armaqmulti','Select Arma(.,q)',min=0,max=10,value=0),
                  ############## DCC GARCH
                  helpText('Choose parameters for conditional correlation'),
                  selectInput('mvdist','Choose Multivariate distribution',c('mvnorm','mvt'),selected='mvnorm'),
                  textInput('testsizedcc','Select test size(in days) for DCC',value='500',placeholder = '500')
              ),
              box(title='Conditional Correlation',width=6,
                  withLoader(plotOutput("plot_dcc",height = 400,width = 650)),solidHeader = TRUE
              ),
              box(title='Conditional Covariance',width=6,
                  withLoader(plotOutput("plot_dcc_var",height = 400,width = 650)),solidHeader = TRUE
              ),
              box(title='Forecast',width=12,
                  withLoader(plotOutput("plot_dcc_forecast",height = 500,width = 1370)),solidHeader = TRUE
              ),
              tabBox(title=span(icon("balance-scale",lib='font-awesome'), "Results"),width=12,
                     tabPanel('DCC-GARCH Estimation',verbatimTextOutput('dccgarchestim')),
                     tabPanel('DCC-GARCH Forecast',verbatimTextOutput('dccgarchforecast'))
              )
              )# FluidRow2
             )# TabItem2
  )#TabItems
)# dashboard Body

# Put them together into a dashboardPage
ui= dashboardPage(skin='black',title='Garch Explorer',
  dashboardHeader(title = span(icon("money-bill-wave",lib='font-awesome'), "GarchExp")),
  sidebar,
  body
)
