#' Intra-day Security Prices
#'
#' @param symbol `character`. A vector of stock symbols.
#' @param interval `character`. Time resolution of price quotes
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#'
#' @examples
#' \donttest{
#'
#' # hourly Apple stock prices
#' d <- fmp_prices('AAPL', interval = '1hour')
#' }
#'
#' @export
fmp_prices <- function(symbol, interval = c('1min', '5min', '15min', '30min', '1hour', '4hour')) {

  interval <- match.arg(interval)
  endpoint <- c('historical-chart', interval)

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint, symbol = symbol)

  #endpoint
  d
}


#' Daily Security Prices
#'
#' @param symbol `character`. A vector of stock symbols.
#' @param start_date `character`. ISO-8601 formatted date. For example, `'2020-05-25'`
#' @param end_date `character`. ISO-8601 formatted date. For example, `'2020-06-25'`
#' @param last_n  `numeric`. An alternative to `start_date` and `end_date`. return the last `n` days
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#' @examples
#' \donttest{
#'
#' # last 100 days of Apple stock prices
#' d <- fmp_prices('AAPL', last_n = 100)
#' }
#'
#' @export
fmp_daily_prices <- function(symbol, start_date = NULL, end_date = NULL, last_n = NULL) {
  endpoint <- 'historical-price-full'

  query_list <- list(
    from = start_date,
    to   = end_date,
    timeseries = last_n
    )

  if (!is.null(start_date) && is.null(end_date)) query_list$to <- Sys.Date()
  if (!is.null(last_n)) {
    query_list$from  <- NULL
    query_list$to  <- NULL
  }

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}

