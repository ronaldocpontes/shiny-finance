library(RColorBrewer)
library(ggthemes)

source('./src/finance/portfolio/portfolio.R', local=TRUE)

DEFAULT_COLORS = brewer.pal(6, "Blues")

benchmark_portifolio = function(df, wght, rebalance, benchmark_column, from, to){
  # Create a dataframe with portfolio and benchmark returns
  df_tmp = df %>% mutate(date = as.Date(row.names(df)))

  # Portfolio return
  port_ret = calcPortReturn(df, from, to, wght, rebalance) %>%
  rename(Portfolio = portfolio.returns)
  port_ret$date = as.Date(row.names(port_ret))

  # Merge into one df
  port_ret = port_ret %>%
    merge(df_tmp[,c(benchmark_column,"date")], by = "date", all.x = TRUE)  %>%
    rename(Benchmark = benchmark_column)

  return(port_ret)
}

calculate_optimal_weights = function(instruments, bt_df, from, to) {

    #Calculate target risk and return
    target_ret = mean(bt_df$Portfolio) * 250
    target_risk = sd(bt_df$Portfolio) * sqrt(250)

    #Extract dataframe for dates
    from = as.Date(from)
    to = as.Date(to)

    df_tmp = instruments %>% rownames_to_column("date") %>%
      filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")

    # Calculate inputs for optimisation
    returns = xts(df_tmp, order.by = as.Date(row.names(df_tmp)))
    mean_ret = apply(df_tmp, 2, mean) * 250
    cov_matrix = cov(df_tmp) * 250

    #Find optimal weights
    opt_w_ret = findEfficientFrontier.ReturnALT(mean_ret, cov_matrix, target_ret)
    opt_w_risk = findEfficientFrontier.Risk(mean_ret, cov_matrix, target_risk)

    #Return a dataframe
    opt_df = data.frame(OptRet = opt_w_ret, OptRisk = opt_w_risk, row.names =  names(instruments))

    return (opt_df)
}

apply_layout = function(x) {
  x %>%
  layout(xaxis = list(title = "", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
          yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE, tickformat = "%"),

          legend = list(orientation = "h", x = 0.1, y=1.2),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          margin = list(b = 20, l = 20, t = 30))
}

plot_allocation_pie_chart = function(weights, labels=names(weights), my_colors = DEFAULT_COLORS ) {

    alloc = data.frame(wght = weights, asset = labels)

    plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#000'),
      hoverinfo = 'text',
      text = ~paste(round(wght,4)*100, ' %'),
      marker = list(colors = my_colors, line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE, width=250, height=250) %>%
      apply_layout()
}

plot_returns_chart = function(bt_df) {
    #Calculate compound return
    bt_df = bt_df %>%
      gather(key="Asset", value="Return", -date) %>%
      group_by(Asset) %>%
      arrange(date) %>%
      mutate(cumRet = cumprod(1+Return) - 1) %>%
      select(date, Asset, cumRet) %>%
      spread(key=Asset, value=cumRet)

    #Plot
    plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
            line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
      add_trace(y= ~Benchmark, name = "Benchmark", line = list(color = "black", width = 2)) %>%
      apply_layout()

}

calculate_performance_metrics = function(ret_df, risk_free, from, to) {
    #Select data

    ret_df = ret_df %>%
      # rename(Mixed = R60T10C30) %>%
      # select(date, Portfolio, Benchmark, Mixed)
      select(date, Portfolio, Benchmark)

    rf_range = risk_free %>% filter(as.Date(date) >= as.Date(from) & as.Date(date) <= as.Date(to))

    #Calculate performance measures
    perf_df = data.frame(Measure = c("Return (annualized), %","Risk (annualized), %","Sharpe","Sortino","Beta","Treynor"))
    perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$Benchmark, rf_range$rf))
    perf_df$Benchmark = unlist(calcPortMeasures(ret_df$Benchmark, ret_df$Benchmark, rf_range$rf))
    # perf_df$Mixed = unlist(calcPortMeasures(ret_df$Mixed, ret_df$Benchmark, rf_range$rf))

    # perf_df[1:2, c("Portfolio","Benchmark","Mixed")] = round(perf_df[1:2, c("Portfolio","Benchmark","Mixed")] * 100, 2)
    perf_df[1:2, c("Portfolio","Benchmark")] = round(perf_df[1:2, c("Portfolio","Benchmark")] * 100, 2)

    return (perf_df)
}

calculate_optimised_metrics = function(ret_df, risk_free, from, to) {
  #Select data
  ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)
  rf_range = risk_free %>% filter(as.Date(date) >= as.Date(from) & as.Date(date) <= as.Date(to))

  #Calculate performance measures
  perf_df = data.frame(Measure = c("Return (annualized), %","Risk (annualized), %","Sharpe","Sortino","Beta","Treynor"))
  perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$Benchmark, rf_range$rf))
  perf_df$Same.Return = unlist(calcPortMeasures(ret_df$Same.Return, ret_df$Benchmark, rf_range$rf))
  perf_df$Same.Risk = unlist(calcPortMeasures(ret_df$Same.Risk, ret_df$Benchmark, rf_range$rf))

  perf_df = perf_df %>% select(Measure, Portfolio, Same.Return, Same.Risk) %>% rename(Similar.Return = Same.Return,
    Similar.Risk = Same.Risk)
  perf_df[1:2, c("Portfolio","Similar.Return","Similar.Risk")] = round(perf_df[1:2, c("Portfolio","Similar.Return","Similar.Risk")] * 100, 2)

  return (perf_df)
}

plot_optimised_returns = function(bt_df) {
  #Calculate compound return
  bt_df = bt_df %>%
  gather(key="Asset", value="Return", -date) %>%
  group_by(Asset) %>%
  arrange(date) %>%
  mutate(cumRet = cumprod(1+Return) - 1) %>%
  select(date, Asset, cumRet) %>%
  spread(key=Asset, value=cumRet)

  #Plot
  plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
    line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
    add_trace(y= ~OptRet, name = "Similar Return", line = list(color = "black", width = 2)) %>%
    add_trace(y= ~OptRisk, name = "Similar Risk", line = list(color = "gray", width = 2)) %>%
    apply_layout()
}

plot_risk_return_ratios = function(df) {
  mean_ret = apply(df, 2, mean) * 250
  sd_ret = apply(df, 2, sd) * sqrt(250)

  df1 = data.frame(Asset = colnames(df), Return = mean_ret, Risk = sd_ret)

  g1 = ggplot(df1, aes(x=Risk, y=Return, label=Asset)) + geom_point(color="steelblue3")  +
    scale_y_continuous(limits=c(0, 0.10), labels = scales::percent_format(accuracy=1)) +
    scale_x_continuous(limits=c(0, 0.4), labels = scales::percent_format(accuracy=1)) +
    xlab('Risk (standard deviation of returns, annualized)') + ylab('Average Returns, annualized') +
    theme_hc()

  g1 = ggplotly(g1, tooltip = c("x","y"), width = 600) %>%   add_annotations(x = df1$Risk, y = df1$Return,
    text = colnames(df), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 60, ay = -30)

  g1$x$data[[1]]$text = paste("Return:", round(df1$Return, 4) * 100, "%","<br>",
                              "Risk:", round(df1$Risk, 4) * 100, "%")

  g1 = g1 %>% layout(margin = list(b = 50, l = 50, t = 100), title = "Risk/Return of Assets <br> (annualized) 2000 - Present")
}

plot_risk_return_by_years = function(df) {
  risk_ret_ann = get_anualised_risk_returns(df)

  g2 = ggplot(risk_ret_ann, aes(x=Risk, y=Return, text = paste(year,"<br>","Return:",
                                                              round(Return,4)*100,"%","<br>", "Risk:", round(Risk,4)*100,"%"))) +
    geom_point(color="steelblue3")  +
    xlab('Risk (standard deviation of returns, annualized)') +
    ylab('') +
    scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
    scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
    theme_hc() + facet_wrap(~reorder(Asset, Risk, sd)) +
    theme(axis.title = element_text(hjust = 1, vjust=1))

  g2 = ggplotly(g2, tooltip = c("text"), width = 600)

  g2[['x']][['layout']][['annotations']][[1]][['y']] = -0.1 #Move y-label lower

  g2 = g2 %>% layout(margin = list(b = 50, l = -50, t = 120), title = "Risk/Return of Assets By Years <br> (annualized) 2000 - Present",
                    yaxis=list(title="Average Return, annualized", tickprefix=" "))
}

plot_compound_returns = function(df) {
  risk_ret_ann = get_anualised_risk_returns(df)
  order = risk_ret_ann %>% group_by(Asset) %>% summarise(sd_SD=sd(Risk)) %>% arrange(sd_SD) %>% select(Asset)
  order = order[['Asset']]


  risk_ret_cum = df %>% mutate(date=rownames(df)) %>%
    gather(key="Asset", value="Return", -date) %>%
    group_by(Asset) %>%
    arrange(date) %>%
    mutate(cumRet = cumprod(1+Return) - 1)

  # Re-arrange
  risk_ret_cum$facet = factor(risk_ret_cum$Asset, levels = c(order))

  g3 = ggplot(risk_ret_cum, aes(x=as.Date(date), y=cumRet, text = paste(date,"<br>", "Compound return:", round(cumRet,4)*100,"%"), group=1)) +
      geom_line(color="steelblue3") + facet_wrap(~facet) +
      scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
      scale_x_date(date_breaks = "5 years", date_labels =  "%y") +
      xlab('Years') + ylab('') + theme_hc()


  g3 = ggplotly(g3, tooltip = "text", width = 600)

  g3[['x']][['layout']][['annotations']][[1]][['y']] = -0.1 #Move y-label lower

  g3 = g3 %>% layout(margin = list(b = 50, l = 50, t = 120), title = "Compound Return <br> 2000 - Present",
                    yaxis=list(title="Compound Return"))
}

plot_efficient_frontier = function(returns, rf) {
  mean_ret = apply(returns, 2, mean) * 250
  cov_matrix = cov(returns) * 250

  sim_port = simPortfolios(mean_ret, cov_matrix, nsim=10000)

  #Calculate the EF line
  min_tret = sim_port[sim_port$Risk==min(sim_port$Risk), "Return"][[1]]  #Usually a good starting point
  max_tret = max(sim_port$Return)

  tret_vector = seq(min_tret, max_tret, length.out = 20)

  ef_line = data.frame(Risk = rep(NA, length(tret_vector)), Return = rep(NA, length(tret_vector)),
                      Portfolio = rep(NA, length(tret_vector))) #Place holder
  i =1 #counter

  for (ret in tret_vector){
    ef_w = findEfficientFrontier.Return(returns, ret)
    tmp.Ret = calcPortPerformance(ef_w, mean_ret, cov_matrix)[[1]]
    tmp.Risk = calcPortPerformance(ef_w, mean_ret, cov_matrix)[[2]]

    ef_line[i,'Return'] = tmp.Ret
    ef_line[i,'Risk'] = tmp.Risk
    ef_line[i, 'Portfolio'] = paste(
      c(colnames(df)),paste(as.character(round(ef_w, 4)*100), "%"), sep=": ", collapse = "<br>")
    i = i+1
  }

  g4 = ggplot(data=sim_port, aes(x=Risk, y=Return)) + geom_point(data=sim_port, aes(x=Risk, y=Return), color='gray', alpha=0.5) +
    geom_line(data=ef_line, aes(x=Risk, y=Return, text = Portfolio, group=1), color='steelblue3', size =2, alpha=0.5) +
    scale_y_continuous(limits=c(0, 0.10), labels = scales::percent_format(accuracy=1)) +
    scale_x_continuous(limits=c(0, 0.25), labels = scales::percent_format(accuracy=1)) +
    theme_hc() + xlab('Risk (standard deviation of returns, annualized)') + ylab('') +
    theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )

  g4 = ggplotly(g4, tooltip = "text", width = 600)

  g4 = g4 %>% layout(margin = list(b = 50, l = 50, t = 120), title = "Simulated Portfolios and the Optimal Line",
                    yaxis = list(title='Average Return, annualized'))
}

get_anualised_risk_returns = function(df) {
  df %>% mutate(date = as.Date(rownames(df))) %>%
    gather(key = "Asset", value="Return", -date) %>%
    mutate(year = year(date)) %>%
    group_by(Asset, year) %>%
    summarize(av_ret = mean(Return)*250, Risk = sd(Return)*sqrt(250) ) %>%
    rename(Return=av_ret)
}
