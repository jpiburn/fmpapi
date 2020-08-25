#' Company Profile
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of relevant financial information
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_profile(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_profile <- function(symbol) {
  endpoint <- "profile"

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Company Stock Splits
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of relevant financial information
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_splits(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_splits <- function(symbol) {
  endpoint <- c('historical-price-full', 'stock_split')

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}


#' Company Stock Dividends
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of relevant financial information
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_dividends(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_dividends <- function(symbol) {
  endpoint <- c('historical-price-full', 'stock_dividend')

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}



#' Company Key Executives
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of relevant financial information
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_key_executives(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_key_executives <- function(symbol) {
  endpoint <- 'key-executives'

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}


#' Company Financial Ratios
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_ratios(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_ratios <- function(symbol, quarterly = FALSE) {
  endpoint <- "ratios"
  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Enterprise Value
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_enterprise_value(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_enterprise_value <- function(symbol, quarterly = FALSE) {
  endpoint <- "enterprise-values"
  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Key Metrics
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_key_metrics(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_key_metrics <- function(symbol, quarterly = FALSE) {
  endpoint <- "key-metrics"
  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Financial Growth
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_financial_growth(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_financial_growth <- function(symbol, quarterly = FALSE) {
  endpoint <- "financial-growth"
  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Rating
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#' @param historical `logical`. If `TRUE` return daily historical values. If `FALSE`
#'        return current estimate. Default is `FALSE`
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_rating(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_rating <- function(symbol, historical = FALSE) {
  endpoint <- "rating"

  if (historical) endpoint <- "historical-rating"

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


#' Company Discounted Cash Flow Value
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#' @param historical `logical`. If `TRUE` return historical dcf values. If `FALSE`
#'        return current estimate. Default is `FALSE`
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_dcf(my_stocks, historical = TRUE, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_dcf <- function(symbol, historical = FALSE, quarterly = FALSE) {
  endpoint <- "discounted-cash-flow"
  query_list <- list(period = NULL)

  if (historical) {
    endpoint <- "historical-discounted-cash-flow"
    if (quarterly) query_list$period <- "quarter"
  }

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls, endpoint = endpoint, historical = historical)

  d
}


#' Upcoming Earnings Calendar
#'
#' @export
#' @examples
#' \donttest{
#' d <- fmp_earnings_calendar()
#' }
fmp_earnings_calendar <- function() {
  endpoint <- "earning_calendar"

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Company Historical Earnings
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @note Currently `fmp_earnings()` requires increased API access
#'
#' @export
fmp_earnings <- function(symbol) {
  endpoint <- c("historical", "earning_calendar")

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


#' Company Market Capitalization
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#' @param historical `logical`. If `TRUE` return daily historical values. If `FALSE`
#'        return current estimate. Default is `FALSE`
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_market_cap(my_stocks, historical = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_market_cap <- function(symbol, historical = FALSE) {
  endpoint <- "market-capitalization"

  if (historical) endpoint <- "historical-market-capitalization"

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint, historical = historical)

  d
}
