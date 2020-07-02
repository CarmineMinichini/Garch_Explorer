library(shiny)

#########################################################################
server <- function(input, output) {
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  addClass(selector = "body", class = "sidebar-collapse")
  
  series = reactive({
    start=input$datestart
    end=input$dateend
    ticker= input$ticker
    
    from.dat <- as.Date(start, format="%d/%m/%y")
    to.dat <- as.Date(end, format="%d/%m/%y")
    
    series = getSymbols(ticker,src="yahoo",from=from.dat,to=to.dat,auto.assign = F) 
    series=series
  })
  output$plot0 <- renderPlotly({
    serie_ticker = series()
    ts_plot(serie_ticker[,4],title=input$ticker,slider=TRUE,color='#23395d',)})
  
  output$plot1 <- renderPlotly({
    serie_ticker = series()
    
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    
    ts_plot(returns,title=input$ticker,slider=TRUE,color='#23395d') })
  #######################################################   SECONDO MENU
  model.spec = reactive({
    garch_type = input$garchtype
    distribution = input$dist
    
    model.spec=ugarchspec(variance.model = list(model = garch_type, garchOrder = c(1, 1),variance.targeting=TRUE), 
                          mean.model = list(armaOrder = c(input$armap, input$armaq),include.mean = T),
                          distribution.model = distribution)
    
  })
  
  output$garchAuto = renderPrint({
    serie_ticker= series()
    
    returns=ROC(serie_ticker[,4],na.pad=FALSE)
    fit_garch=garchAuto(returns,cores = 1,cond.dists = input$dist)
    print(fit_garch@formula)
  
  })
  
  output$plot2 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=1) })
  
  output$plot3 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=2) })
  
  output$plot4 <- renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    plot(modelfit,which=8) })
  ################################################## TERZO MENU 
  output$backtest <- renderPrint({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    report(model.roll, type = "VaR",VaR.alpha = c(0.01, 0.05)) })
  
  
  output$plot5 = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    plot(model.roll,which=3) })
  
  output$plot6 = renderPlot({
    specifiche = model.spec()
    serie_ticker= series()
    returns=(dailyReturn(Cl(as.xts(serie_ticker)),type="log"))
    modelfit=ugarchfit(spec=specifiche ,data=returns,out.sample=0)
    
    
    model.roll = ugarchroll(spec = specifiche, data = returns, 
                            n.ahead = 1, forecast.length = as.integer(input$testsize), n.start = NULL, 
                            refit.every = 50, refit.window = c("recursive"), 
                            window.size = NULL, solver = "hybrid", calculate.VaR = TRUE, 
                            VaR.alpha = c(0.01, 0.05), cluster = NULL, keep.coef = TRUE)
    
    plot(model.roll,which=2) })
  
  
  
  wallet = reactive({
    
    start1=input$datestart1
    end1=input$dateend1
    ticker1= input$ticker1
    ticker2= input$ticker2
    
    quota1= input$quota1
    quota2= input$quota2
    
    from.dat <- as.Date(start1, format="%d/%m/%y")
    to.dat <- as.Date(end1, format="%d/%m/%y")
    
    first_ticker = getSymbols(ticker1,src="yahoo",from=from.dat,to=to.dat,auto.assign = F) 
    second_ticker = getSymbols(ticker2,src="yahoo",from=from.dat,to=to.dat,auto.assign = F) 
    
    wallet =first_ticker[,4]*as.numeric(quota1) + second_ticker[,4]*as.numeric(quota2)
    
    })
  
  output$plot_wallet  <- renderPlotly({
    pf = wallet()
    returns=CalculateReturns(pf,method='log')
    ts_plot(returns,title='Wallet returns',slider=TRUE,color='#23395d')})  
  
  wallet2 = reactive({
    
    start1=input$datestart1
    end1=input$dateend1
    ticker1= input$ticker1
    ticker2= input$ticker2
    
    quota1= input$quota1
    quota2= input$quota2
    
    from.dat <- as.Date(start1, format="%d/%m/%y")
    to.dat <- as.Date(end1, format="%d/%m/%y")
    
    first_ticker = getSymbols(ticker1,src="yahoo",from=from.dat,to=to.dat,auto.assign = F) 
    second_ticker = getSymbols(ticker2,src="yahoo",from=from.dat,to=to.dat,auto.assign = F) 
    
    pf = merge(first_ticker[,4],second_ticker[,4])
    wallet2=na.approx(na.trim(CalculateReturns(pf,method='log')))
    })
  
  output$plot_dcc  <- renderPlot({
    pf = wallet2()
    specifiche= model.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = input$mvdist)
    
    fit1 = dccfit(spec1, data = pf, fit.control = list(eval.se = TRUE), fit = multf)
    
    plot(fit1,which=4)
    
    })
  
  output$plot_dcc_forecast <- renderPlot({
    pf = wallet2()
    specifiche= model.spec()
    
    uspec.n = multispec(replicate(2,specifiche))
    multf = multifit(uspec.n,pf)
    
    spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = input$mvdist)
    
    fit01 = dccfit(spec1, data = pf, out.sample = as.integer(input$testsizedcc), fit.control = list(eval.se=FALSE))
    forc2 = dccforecast(fit01, n.ahead = 1, n.roll = as.integer(input$testsizedcc) )
    
    plot(forc2, which=5)
  })
  

  
  
  
  
}