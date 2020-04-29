source("./src/datasources/datasources.R", local=TRUE)
source("./src/shiny/finance/portfolio/portfolio_server.R", local=TRUE)
source("./app_layout.R", local=TRUE)

DISCLAIMER_PAGE  = renderUI(includeHTML("./html/disclaimer.html"))
AUTHOR_PAGE  = renderUI(includeHTML("./html/author.html"))

CACHE_FOLDER = "data"
INSTRUMENTS = load_instruments(CACHE_FOLDER)

RISK_FREE_RATES = c("DGS3MO", "USD3MTD156N", "USDONTD156N", "GBP3MTD156N", "GBPONTD156N", "EUR3MTD156N", "EURONTD156N")
INITIAL_SELECTION = c("^GSPC","^DJI","^IXIC","^FTSE","^N100", "AAPL")
RISK_FREE = "DGS3MO"

ANALYSIS_PERIOD = seq(as.Date("2010-01-01"), Sys.Date(), by="1 week")
START_DATE = first(ANALYSIS_PERIOD)
END_DATE = last(ANALYSIS_PERIOD)

shinyApp(
  ui = app_layout(INSTRUMENTS, INITIAL_SELECTION, RISK_FREE_RATES, RISK_FREE, ANALYSIS_PERIOD),

  server = function(input, output, session) {
    render_portfolio_optmisation(input, output, session, RISK_FREE_RATES, START_DATE, END_DATE, CACHE_FOLDER)
    output$author = AUTHOR_PAGE
    output$disclaimer = DISCLAIMER_PAGE
  }
)
