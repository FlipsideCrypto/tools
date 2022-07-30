
#author: eric @ flipside crypto
# twitter: @theericstone

# this is R code that will walk thru a basic approach 
# for using MCMC to make price projections based on
# a skew-normal disatribution of daily crypto returns

# this is quite similar to the approach i used to track do kwon's 
# infamous bet that the doomed LUNA token would be > 90ish in mar 2023

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
### THIS requires a free flipside api key to run queries ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

# flipside crypto's massive open dataset via R
# (free to all: https://sdk.flipsidecrypto.xyz/shroomdk)
# follow the setup steps for the R SDK 
#   -> here https://docs.flipsidecrypto.com/shroomdk-sdk/sdks/r
# once you have that set up this should run!

#load required packages
#one-time setup for shroomDK
if(FALSE){
  library(devtools) # install if you haven't already
  devtools::install_github(repo = 'FlipsideCrypto/sdk', subdir = 'r/shroomDK')
  #remember to save your api key to GIT IGNORED location
  #and create an object to refer to it
  api_key <- readLines("shroomdk.key")
  
  #this helper function is the most straightforward way to query the database
  #  --> shroomDK::auto_paginate_query(query, api_key) # <- pulls up to 1M rows of a query & attempts to make it a nice data frame. 
  #explore the data in a sql editor at https://flipside.new
}

library(shroomDK)
library(magrittr)
library(sde)
library(sn)
library(grid)
library(quantmod)
library(data.table)
api_key <- readLines("shroomdk.key")

options(scipen = 999)

price.data <- data.table(
  shroomDK::auto_paginate_query(
    "select 
    hour::date as date,
    median(price) as price
    from ethereum.core.fact_hourly_token_prices
    where hour > current_date - 183
    and token_address = lower('0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2')
    and symbol = 'WETH'
    group by 1
    order by 1 desc;",
    api_key
  )
)
#here's the same query in the flipside app:
#https://app.flipsidecrypto.com/velocity/queries/6ca54b23-6336-45f5-b466-cd03eddbbcc9

#the simulation function is expecting these columns
setnames(price.data, c("date","price_close"))
setorder(price.data, date)
price.data[,symbol := "WETH"]

#this function sets up a basic mcmc approach
# to draw samples of daily returns for the price data you feed in

MarkovChainSimulationsSN <- function(
  pastRange = c(Sys.Date() - 183, Sys.Date()), #how far back should we look for context setting
  nYears = .5, #in years -- how far ahead do we forecast
  nSimulations = 1000,
  timeperiod = c("days"), #simulation period
  principal = NULL, #optional for portfolio tracking
  plotSimulations = FALSE, #plot the simulations summary?
  includePortfolio = TRUE, #optional for portfolio tracking
  prices = price.data
){
  n.walks <- ceiling(nYears * switch(timeperiod, weeks = 52, days = 365, months = 12))
  
  prices[price_close > 0,first_trade := min(date) ]
  
  by.var.list <- c("date","symbol","price_close")
  by.list <- "symbol,period"
  by.var <- "symbol"
  
  prices <- prices[ !is.na(first_trade) ][,by.var.list, with = FALSE ]
  prices[,period := switch(
    timeperiod,
    "days" = date,
    round(as.IDate(date),timeperiod)),
    by = by.var ]
  prices <- prices[,list(mean_price = mean(price_close)),by = by.list ]
  prices[,period_return := Delt(mean_price,k = 1), by = by.var ]
  prices[ is.na(period_return) ]$period_return <- 0
  
  #calculate skew normal parameters by MLE
  sn.pars <- lapply(unique(prices[[ by.var ]]), function(.sym){
    to.return <- switch(
      by.var,
      symbol = try({
        model.fit <- st.mple(y = prices[ symbol == .sym]$period_return,
                             opt.method = "BFGS")
        if(model.fit$opt.method$convergence != 0) error("Model did not converge!")
        data.table(
          symbol = .sym,
          as.data.table(t(model.fit$dp))
        )
      },silent = TRUE),
      asset_id = try({
        model.fit <- st.mple(y = prices[ asset_id == .sym]$period_return,
                             opt.method = "BFGS")
        if(model.fit$opt.method$convergence != 0) error("Model did not converge!")
        data.table(
          asset_id = .sym,
          as.data.table(t(model.fit$dp))
        )
      },silent = TRUE)
    )
    
    if(class(to.return)[1] == "try-error"){
      if(by.var == "symbol") return(data.table(symbol = .sym)) else
        return(data.table(asset_id = .sym))
    } else return(to.return)
  }) %>% rbindlist(fill = TRUE)
  sn.pars <- sn.pars[ !is.na(xi) ]
  
  future.dates <- seq.Date(as.Date(pastRange[2]) + 1,as.Date(pastRange[2]) + 1 + (365 * nYears),by = timeperiod)[1:(n.walks + 1)]
  if(length(unique(prices[[ by.var ]])) > 10){
    sim.prog <- txtProgressBar(max = length(unique(prices[[ by.var ]])),style = 3)
  }
  all.simulations <- lapply(unique(prices[[ by.var ]]), function(.sym){
    if(length(unique(prices[[ by.var ]])) > 10){
      setTxtProgressBar(sim.prog, value = which(unique(prices[[ by.var ]]) == .sym))
    }
    init <- if(is.null(principal)){
      switch(
        by.var,
        symbol = {prices[ symbol == .sym ]$mean_price %>% tail(1)},
        asset_id = {prices[ asset_id == .sym ]$mean_price %>% tail(1)}
      )
    } else {
      principal
    }
    simulations <- lapply(1:nSimulations,function(.n){
      sample.sn <- switch(
        by.var,
        symbol = rsn(
          n = n.walks,
          xi    = sn.pars[ symbol == .sym ]$xi,
          omega = sn.pars[ symbol == .sym ]$omega,
          alpha = sn.pars[ symbol == .sym ]$alpha),
        asset_id = rsn(
          n = n.walks,
          xi    = sn.pars[ asset_id == .sym ]$xi,
          omega = sn.pars[ asset_id == .sym ]$omega,
          alpha = sn.pars[ asset_id == .sym ]$alpha)
      )
      returns <- c(init,1 + sample.sn)
      switch(
        by.var,
        symbol = data.table(
          symbol = .sym,
          simulation = .n,
          date = future.dates,
          return = cumprod(returns)
        ),
        asset_id = data.table(
          asset_id = .sym,
          simulation = .n,
          date = future.dates,
          return = cumprod(returns)
        )
      )
    }) %>% rbindlist()
  }) %>% rbindlist()
  
  by.list.date <- switch(
    by.var,
    asset_id = "asset_id,date",
    symbol = "symbol,date"
  )
  sim.summary <- all.simulations[,list(mean = mean(return),
                                       median = median(return),
                                       precentile_05 = quantile(return,probs = .05),
                                       percentile_95 = quantile(return,probs = .95),
                                       min = min(return),
                                       max = max(return)), by = by.list.date ]
  
  if(includePortfolio){
    if(!exists("allocation") & length(unique(sim.summary$symbol)) == 1){
      allocation <- 1
      symbols <- unique(sim.summary$symbol)
    } 
    portfolio.sim <- 
      merge(all.simulations,
            data.table(symbol = symbols, weight = allocation),
            by = "symbol")[,list(return = weighted.mean(return,weight)), by = "simulation,date"]
    
    portfolio.sim.summary <- portfolio.sim[,list(mean = mean(return),
                                                 median = median(return),
                                                 precentile_05 = quantile(return,probs = .05),
                                                 percentile_95 = quantile(return,probs = .95),
                                                 min = min(return),
                                                 max = max(return)), by = "date" ]
  } else {
    portfolio.sim <- "No portfolio simulations run"
    portfolio.sim.summary <- "No portfolio simulations run"
  }
  
  if(plotSimulations){
    sim.melt <- data.table(melt(rbindlist(list(
      sim.summary,
      portfolio.sim.summary[,symbol := "Portfolio"]),use.names = TRUE, fill = FALSE),
      id.vars = c("symbol","date")))
    sim.melt[,log_value := log(value,base = 10)]
    sim.melt <- melt(sim.melt, id.vars = c("symbol","date","variable"),variable.name = "transformation")
    print(ggplot(sim.melt, aes(x = date, y = value, group = variable, color = factor(variable))) +
            geom_line() +
            facet_wrap(transformation ~ symbol,scale = "free_y",nrow = 2) +
            theme_minimal())
  }
  
  return(list(
    individual = list(
      all = all.simulations,
      summary = sim.summary
    ),
    portfolio = list(
      all = portfolio.sim,
      summary = portfolio.sim.summary
    )
  ))
}

#today's summary
#set.seed(420.69) this is unnecessary unless you need to replicate results
#let's run 1000 siulations ahead to 6 months from now
#based on the last 60 days of WETH prices

price.sims <- MarkovChainSimulationsSN(
  pastRange = c(Sys.Date() - 60,Sys.Date()),
  nYears = .5,
  nSimulations = 1000
)

#now we can look at fun probatilities of a particular price for instance...
per.sims <-  nrow(price.sims$individual$all[ date == "2023-01-01" & return >= 3000 ]) /
  nrow(price.sims$individual$all[ date == "2023-01-01" ])
sims.text <- paste("prob ETH > $3000 by jan 1: ",per.sims*100,"% 😬")

#here's one way to plot the price fan
library(ggplot2)
ggplot(
  price.sims$individual$all,
  aes(x = date, y = return,
      group = as.factor(simulation),
      color = as.factor(simulation))) +
  geom_line(alpha = .2) +
  scale_y_log10() +
  geom_text(data = data.table(text = sims.text, date = as.Date("2022-10-14"), simulation = as.factor(1), return = 8000),
            mapping = aes(label = text),
            fontface = "bold", color = "blue", size = 5) +
  geom_hline(yintercept = 3000, color = "blue", size = 1.4, alpha = .4) +
  theme_light() + 
  theme(legend.position = "none", axis.text.x = element_text(hjust = 1)) +
  ylab("Modeled $WETH Price")

# you can get a lot fancier with this obvi,
# but i hope this is a decent starting point, enjoy!
