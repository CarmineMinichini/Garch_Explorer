library(shiny)
format(Sys.Date(),"%d/%m/%y")
#########################################################################
server <- function(input, output) {
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  addClass(selector = "body", class = "sidebar-collapse")
  
  series = reactive({
    start=input$daterange[1]
    end=input$daterange[2]
    ticker= input$ticker
    
    series = getSymbols(ticker,src="yahoo",from=start,to=end,auto.assign = F) 
    series=series
  })
  ##################################################### CLOSE PRICES
  output$plot0 <- renderPlotly({
    serie_ticker = series()
    ts_plot(serie_ticker[,4],title=input$ticker,slider=TRUE,color='black',)})
  
  ##################################################### Returns
  output$plot1 <- renderPlotly({
    serie_ticker = series()
    
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    returns=na.omit(returns)
    
    ts_plot(returns,title=input$ticker,slider=TRUE,color='black') })
  ##################################################### ACF
  output$plotacf <- renderPlotly({
    serie_ticker = series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    acf <- acf(returns, plot = FALSE)
    bacfdf <- with(acf, data.frame(lag, acf))
    
    q <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
      geom_bar(stat = "identity", position = "identity",fill='black',color='red') + geom_smooth() +
      labs(x='Lag',y='ACF',title='ACF') + theme_classic()
    ggplotly(q)
    })
  ##################################################### Partial ACF
  output$plotpacf <- renderPlotly({
    serie_ticker = series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    pacf <- pacf(returns, plot = FALSE)
    bacfdf <- with(pacf, data.frame(lag, acf))
    
    q <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
      geom_bar(stat = "identity", position = "identity",fill='#23395d',color='red') + geom_smooth() +
      labs(x='Lag',y='PACF',title='PACF') + theme_classic()
    ggplotly(q)
  })
  ####################################################### GARCH REACTIVE
  model.spec = reactive({
    garch_type = input$garchtype
    distribution = input$dist
    
    model.spec=ugarchspec(variance.model = list(model = garch_type, garchOrder = c(1, 1),variance.targeting=TRUE), 
                          mean.model = list(armaOrder = c(input$armap, input$armaq),include.mean = T),
                          distribution.model = distribution)
    
  })
  ####################################################### AutoGARCH
  output$garchAuto = renderPrint({
    serie_ticker= series()
    
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    fit_garch=garchAuto(returns,cores = 1,cond.dists = input$dist)
    print(fit_garch@formula)
  
  })
  ####################################################### PLOT 1 MODELFIT
  output$plot2 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=1) })
  ####################################################### PLOT 2 MODELFIT
  
  output$plot3 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=2) })
  ####################################################### PLOT 3 MODELFIT
  
  output$plot4 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=8) })
  ################################################## PLOT ROLLING FORECAST RETURNS
  output$plot5 = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    plot(model.roll,which=3) })
  ################################################## PLOT ROLLING FORECAST VOLATILITY
  output$plot6 = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    plot(model.roll,which=2) })
  ####################################################################### VAR BACKTEST
  output$backtest <- renderPrint({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    report(model.roll, type = "VaR",VaR.alpha = c(0.01, 0.05)) })
  
  ####################################################################### VAR PLOT
  
  output$varplot = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    plot(model.roll,which=4)
  })
  ####################################################################### FORECAST RETURNS 1 AHEAD
  output$plotfrcst = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll= 0, out.sample =0)
    
    plot(modelfor,which=1)
    
  })
  ####################################################################### FORECAST VOLATILITY 1 AHEAD
  output$plotfrcstvol = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll= 0, out.sample =0)
    
    plot(modelfor,which=3)
    
  })
  ####################################################################### FORECAST VOLATILITY 1 AHEAD
  output$garchforecast = renderPrint({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll= 0, out.sample =0)
    
    print(modelfor)
    
  })
  
  output$garchestim = renderPrint ({
    specifiche = model.spec()
    serie_ticker= series()
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)

    print(modelfit)
    
  })
  
  ###################################################################
  ###################################################################
  ################################################################### PORTAFOGLIO MENU
  
  wallet = reactive({
    
    start=input$daterange[1]
    end=input$daterange[2]
    ticker1= input$ticker1
    ticker2= input$ticker2
    
    quota1= input$quota1
    quota2= input$quota2
    
    first_ticker = getSymbols(ticker1,src="yahoo",from=start,to=end,auto.assign = F) 
    second_ticker = getSymbols(ticker2,src="yahoo",from=start,to=end,auto.assign = F) 
    
    pf = merge(first_ticker[,4],second_ticker[,4])
    wallet = na.approx(na.trim(CalculateReturns(pf,method='log')))
    })
  
  ####################################################################### SPECIFICAZIONE PORTAFOGLIO
  multimodel.spec = reactive({
    garch_type = input$garchtypemulti
    distribution = input$distmulti
    
    model.spec=ugarchspec(variance.model = list(model = garch_type, garchOrder = c(1, 1),variance.targeting=TRUE), 
                          mean.model = list(armaOrder = c(input$armapmulti, input$armaqmulti),include.mean = T),
                          distribution.model = distribution)
    
  })
  ############################################################ CONDITIONAL CORRELATION
  output$plot_dcc  <- renderPlot({
    pf = wallet()
    specifiche= multimodel.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model='aDCC',distribution = input$mvdist)
    
    fit1 = dccfit(spec1, data = pf, fit.control = list(eval.se = TRUE), fit = multf)
    
    plot(fit1,which=4)
    
    })
  
  ############################################################ CONDITIONAL VARIANCES
  output$plot_dcc_var  <- renderPlot({
    pf = wallet()
    specifiche= multimodel.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model='aDCC',distribution = input$mvdist)
    
    fit1 = dccfit(spec1, data = pf, fit.control = list(eval.se = TRUE), fit = multf)
    
    plot(fit1,which=3)
    
  })
  ############################################################ FORECAST EW PORTFOLIO
  output$plot_dcc_forecast <- renderPlot({
    pf = wallet()
    specifiche= multimodel.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model='aDCC',distribution = input$mvdist)
    
    fit01 = dccfit(spec1, data = pf, out.sample = as.integer(input$testsizedcc), fit.control = list(eval.se=FALSE))
    forc2 = dccforecast(fit01, n.ahead = 1, n.roll = as.integer(input$testsizedcc) )
    
    plot(forc2, which=5)
  })
  
  ############################################################ FORECAST CORRELATION
  output$dccgarchestim = renderPrint({
    
    pf = wallet()
    specifiche= multimodel.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model='aDCC',distribution = input$mvdist)
    
    fit1 = dccfit(spec1, data = pf, fit.control = list(eval.se = TRUE), fit = multf)
    
    print(fit1)
    
    })
  
  output$dccgarchforecast = renderPrint({
    
    pf = wallet()
    specifiche= multimodel.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model='aDCC',distribution = input$mvdist)
    
    fit01 = dccfit(spec1, data = pf, out.sample = as.integer(input$testsizedcc), fit.control = list(eval.se=FALSE))
    forc2 = dccforecast(fit01, n.ahead = 1, n.roll = as.integer(input$testsizedcc) )
    
    print(forc2)
    
    
  })
  
  
}