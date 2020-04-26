library(xts)
library(DEoptim)
library(tseries)
library(tibble)
library(PerformanceAnalytics)

### Helper functions for portfolio valuation
decimals = function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1}

is_round_of = function(x, y) { x == round(y, digits = decimals(x))}

# Rebalance weights vector given a change so the sum is 1
rebalance = function(weights, change, key) {
  current = weights[[key]]
  if (is_round_of(change, current)) {
    return(weights)
  } else if (change==1){
    change = 0.9999
  }
  i = match(key, names(weights))
  delta = change - current
  weights = weights - (delta * weights / sum(weights[-i]))
  weights[[key]] = change
  weights
}

# Function that calculates portfolio return/risk given mean returns of assets and a covariance matrix
calcPortPerformance = function(weights, mean_ret, cov_matrix){
  portRet =  t(weights) %*% mean_ret
  portRisk =  sqrt(t(weights) %*% (cov_matrix %*% weights))
  return (list(portRet, portRisk))
}

## Function that simulates portfolio risk/return given mean returns of assets and a covariance matrix
simPortfolios = function(mean_ret, cov_matrix, nsim=10000){

    n_assets = length(mean_ret) #Get number of assets

    #Create empty DataFrame
    result = data.frame(Return = rep(NA,nsim), Risk = rep(NA, nsim))

    #Simulate portfolios performance and populate the resulting dataframe
    for (i in 1:nsim){
      weights = runif(n_assets, 0, 1)  #Simulate normal distribution
      weights = weights/sum(weights)      #Make sure that weights add up to 1.0

      portRet = calcPortPerformance(weights, mean_ret, cov_matrix) [[1]]
      portRisk = calcPortPerformance(weights, mean_ret, cov_matrix) [[2]]

      result$Return[i] = portRet
      result$Risk[i] = portRisk
      }
    return (result)
}

## Function that finds weights of assets on the efficient frontier
# By target return
findEfficientFrontier.Return = function(returns, target_ret, short = FALSE){

    #Calculate optimal weights
    opt.weights = portfolio.optim(returns, pm=target_ret/250, shorts = short)$pw

    if (short == FALSE){
      opt.weights = pmax(opt.weights, 0) #Correct approximation error
      opt.weights = opt.weights/sum(opt.weights)
    }
    opt.weights
}

#By target risk
findEfficientFrontier.Risk = function(mean_ret, cov_matrix, target_risk){

  obj_func = function(w){

    #To avoid NA
    if (sum(w) ==0){
      w = w + 1e-10}

    #Balance to one
    w = w/sum(w)

    #Calculate negative return
    neg_ret = - t(w) %*% mean_ret
    p_risk = sqrt(t(w) %*% cov_matrix %*% w)

    (neg_ret + abs(p_risk - target_risk)) #Penalized optimisation
  }

  # Set parameters
  controlDE <- list(reltol=1e-7,steptol=100, itermax = 10000,trace = 5000, strategy=6, c=0)

  #Long only
  N = length(mean_ret)
  lower = rep(0,N)
  upper = rep(1,N)

  out <- DEoptim(fn = obj_func, lower = lower, upper = upper, control = controlDE)
  opt_w = out$optim$bestmem

  opt_w/sum(opt_w) #Sum up to 1
}

#By target return ALTERNATIVE
findEfficientFrontier.ReturnALT = function(mean_ret, cov_matrix, target_ret){

  obj_func = function(w){

    #To avoid NA
    if (sum(w) ==0){
      w = w + 1e-10}

    #Balance to one
    w = w/sum(w)

    #Calculate negative return
    port_ret =  t(w) %*% mean_ret
    port_risk = sqrt(t(w) %*% cov_matrix %*% w)

    return(port_risk + abs(port_ret - target_ret)*10) #Penalized optimisation
  }

  # Set parameters
  controlDE <- list(reltol=1e-7,steptol=100, itermax = 10000,trace = 5000, strategy=6, c=0)
  #Long only
  N = length(mean_ret)
  lower = rep(0,N)
  upper = rep(1,N)

  out <- DEoptim(fn = obj_func, lower = lower, upper = upper, control = controlDE)

  opt_w = out$optim$bestmem
  opt_w/sum(opt_w) #Sum up to 1
}

#Function that calculates portfolio returns
calcPortReturn = function(df, from, to, weights, rebalance, geometric = TRUE){
  #Cut dataframe to reflect date range
  df_range = df %>% drop_na() %>% rownames_to_column("date") %>%
    filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")

  df_range = xts(df_range, order.by = as.Date(row.names(df_range)))
  tclass(df_range) <- "Date"

  #Create repalace operator
  reb_op = ifelse(rebalance=="Never", NA,
                  ifelse(rebalance=="Annually", "years",
                  ifelse(rebalance=="Quarterly", "quarters",
                         "months")))

  Return.portfolio(df_range,
    weights = weights, geometric = geometric,rebalance_on = reb_op) %>%
    data.frame()
}

#### Function to find optimal portfolios
opt_port = function(df, from, to, opt_w, port_ret){

  #Get portfolio  returns
  port_ret = port_ret %>% select(date, Portfolio, Benchmark)

  df_tmp = df %>% rownames_to_column("date") %>%
    filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")

  #Same return portfolio
  opt_ret = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRet, rebalance = "Never" , geometric = FALSE))
  opt_ret$date = as.Date(row.names(opt_ret))

  #Same risk portfolio
  opt_risk = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRisk, rebalance = "Never", geometric = FALSE))
  opt_risk$date = as.Date(row.names(opt_risk))


  #Combine into one dataframe
  port_ret = merge(port_ret, opt_ret, by = "date", all.x = TRUE)
  port_ret = merge(port_ret, opt_risk, by = "date", all.x = TRUE)
  port_ret$date = as.Date(port_ret$date)

  #Change names
  colnames(port_ret) = c("date","Portfolio","Benchmark" , "OptRet","OptRisk")

  return(port_ret)
}

#Function that calculates portfolio performance measures

calcPortMeasures = function (port_ret, benchmark, rf){
  mean_rf = mean(rf)
  mean_port_ret = mean(port_ret)
  sd_port_ret = sd(port_ret)

  #Calculate Sharpe
  sharpe = ((mean_port_ret - mean_rf) / sd_port_ret) * sqrt(250)

  #Calculate Beta
  mod = lm(formula = port_ret~benchmark)
  beta = summary(mod)$coefficients[2,1]

  #Calculate Sortino
  sortino = SortinoRatio(port_ret) * sqrt(250)

  #Calculate Taylor
  treynor = ((mean_port_ret - mean_rf)*250)*100/beta

  list("AvRet"=mean_port_ret * 250, "StDev" = sd_port_ret * sqrt(250),
                 "Sharpe" = sharpe, "Sortino" = sortino[1], "Beta" = beta, "Treynor" = treynor)
}
