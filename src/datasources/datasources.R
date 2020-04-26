library(tidyquant)
library(BatchGetSymbols)

load_instruments = function(cache_folder) {
  c("^GSPC","^DJI","^IXIC","^FTSE","^N100","^TNX") %>%
  append(GetSP500Stocks(do.cache=TRUE, cache.folder = cache_folder)$Tickers)
}

load_tickers = function(symbols, start_date, end_date, cache_folder) {

  TICKERS = BatchGetSymbols(
    tickers=symbols,
    first.date=start_date,
    last.date=end_date,
    freq.data = "daily",
    type.return="log",
    do.complete.data = FALSE,
    do.fill.missing.prices = TRUE,
    do.cache=TRUE,
    do.parallel=FALSE,
    cache.folder=cache_folder)

  df = TICKERS$df.tickers %>%
            select(c(ticker, ref.date, ret.adjusted.prices)) %>%
            spread(ticker, ret.adjusted.prices) %>%
            drop_na() %>%
            data.frame(row.names=.$ref.date) %>%
            select(-ref.date)
}

load_fred_data = function (symbols, start_date, end_date) {
  tq_get(symbols, get = "economic.data", from=start_date, to=end_date) %>%
    spread(symbol, price) %>%
    drop_na() %>%
    data.frame(row.names=.$date) %>%
    select(-date)
}

fred_to_daily_rates = function(a) { (a/100 + 1) ^ (1/360) -1}
