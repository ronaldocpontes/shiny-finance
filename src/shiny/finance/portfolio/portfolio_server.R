library(tidyr)
library(purrr)
library(glue)
library(tibble)

source('./src/datasources/datasources.R', local=TRUE)
source('./src/shiny/shiny_utils.R', local=TRUE)
source('./src/finance/portfolio/portfolio_reporting.R', local=TRUE)

HOW_TO_USE =
  renderUI(includeHTML("./src/shiny/finance/portfolio/how_to_use.html"))

PERFORMANCE_MEASURES =
  renderUI(includeHTML("./src/shiny/finance/portfolio/performance_measures.html"))

render_portifolio_optmisation = function(input, output, session, risk_free_choices, start_date, end_date, cache_folder) {
  react = c()

  # Download instruments and render selectors
  rfr = load_fred_data(risk_free_choices, start_date, end_date) %>%
        fred_to_daily_rates()

  react$risk_free = reactive({
    data.frame(date=rownames(rfr), rf=rfr[[input$risk_free]])
  })

  react$instruments = reactive({
    if (length(input$tickers > 1))
      load_tickers(toupper(input$tickers), start_date, end_date, cache_folder)
  })

  output$benchmark = renderUI({selectizeInput("benchmark",
    label = "Benchmark",
    choices = input$tickers,
    selected = input$tickers[1],
    multiple = FALSE)})

  # Update UI on selection changes
  instrument_changes <- reactive({
    list(input$tickers, input$benchmark)
  })

  observeEvent(instrument_changes(),{
    render_portfolio_ui(output, input, session, react)
  })

  output %>%
    render_theory_page(react$instruments(), react$risk_free())
}

render_portfolio_ui = function(output, input, session, react, rv) {
  instrument_names = react$instruments() %>% colnames()
  ni =  length(instrument_names)

  if(ni <  2 || is.null(input$benchmark) || nchar(input$benchmark) < 1) {
    return(output)
  }

  rv = reactiveValues(selectedWeights = rep(1/ni,ni))
  names(rv$selectedWeights) = instrument_names

  react$bt_data = reactive({
    rv$selectedWeights
    input$date_range
    input$rebalance

    isolate(benchmark_portifolio(
      react$instruments(),
      rv$selectedWeights,
      input$rebalance,
      gsub("\\^", "X.", input$benchmark),
      as.Date(input$date_range[1]),
      as.Date(input$date_range[2])))
  })

  render_sliders(input, output, session, react, rv)
  render_portifolio_returns(input, output, session, react, rv)

  render_optimisation_page(input, output, session, react, rv)

  output
}

render_sliders = function(input, output, session, react, rv) {

  #Allocation sliders
  output$sliders = renderUI({
    sliders = names(rv$selectedWeights) %>%
      imap(~ { wghtsliderInput(glue("{sliders_id}-{.}"), rv$selectedWeights[.y], label = .) }) %>%
      split(rep_len(1:2, length(.))) %>%
      map(~ {column(6, .)})

    tags$div(id="sliders-container", sliders)
  })

  # Sliders Observers: If any of the sliders change, rebalance other weights so sum = 1
  sliders_id = "multi_slider"
  slider_observers = names(rv$selectedWeights) %>% imap( ~ {
    slider_input = glue("{sliders_id}-{.}")
    observeEvent(input[[slider_input]], {
      suspendMany(slider_observers)
      current = rv$selectedWeights[[.]]
      change = input[[slider_input]]
      if (!is_round_of(change, current)) {
        rv$selectedWeights = rebalance(rv$selectedWeights, input[[slider_input]], .)
      }
      resumeMany(slider_observers)
    })
  })
}

render_portifolio_returns = function(input, output, session, react, rv) {

  # Allocation pie chart
  output$graph5 = renderPlotly({ plot_allocation_pie_chart(rv$selectedWeights) })

  # Returns and metric table
  output$graph6 = renderPlotly({ plot_returns_chart(react$bt_data()) })

  output$bt_table1 = renderTable(digits =2, {
    calculate_performance_metrics(react$bt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })
}

render_optimisation_page = function(input, output, session, react, rv) {

  react$opt_weights = reactive({
    calculate_optimal_weights(react$instruments(), react$bt_data(), input$date_range[1], input$date_range[2])})

  # Optimised pie charts
  output$graph7 = renderPlotly({
    plot_allocation_pie_chart(rv$selectedWeights) })

  output$graph8 = renderPlotly({
    df = react$opt_weights()
    plot_allocation_pie_chart(df$OptRet, rownames(df)) })

  output$graph9 = renderPlotly({
    df = react$opt_weights()
    plot_allocation_pie_chart(df$OptRisk, rownames(df)) })

  react$opt_data = reactive({
    opt_port(
      react$instruments(),
      as.Date(input$date_range[1]),
      as.Date(input$date_range[2]),
      react$opt_weights(),
      react$bt_data())})

  # Optimised Returns and metric table
  output$graph10 = renderPlotly({
    plot_optimised_returns(react$opt_data())
  })

  output$bt_table2 = renderTable(digits=2, {
    calculate_optimised_metrics(react$opt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })
}

render_theory_page = function(output, df, rf) {
  output$how_to_use = HOW_TO_USE
  output$measures = PERFORMANCE_MEASURES

  output$graph1 = renderPlotly({plot_risk_return_ratios(df)})
  output$graph2 = renderPlotly({plot_risk_return_by_years(df)})
  output$graph3 = renderPlotly({plot_compound_returns(df)})
  output
}
