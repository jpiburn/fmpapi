#' Quote Available Securities
#'
#' Group of functions for retrieving groups of related securities
#'
#' Group of functions Details paragraph.
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @section After Arguments and Value sections:
#' Despite its location, this actually comes after the Arguments and Value sections.
#' Also, don't need to use null, could annotate first function, and then
#' using function name as the groupBy name is more intuitive.
#'
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#' @examples
#'
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_quote(my_stocks)
#'
#' @export
fmp_quote <- function(symbol) {
  endpoint <- "quote"

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


#' @noRd
bulk_quote_endpoint <- function(endpoint) {
  endpoint <- c('quotes', endpoint)

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Retrieve Quotes from New York Stock Exchange
#'
#' @export
#' @rdname fmp_quote
#' @examples
#'
#' # real-time quote of every stock on nyse
#' df_nyse <- fmp_quote_nyse()
fmp_quote_nyse <- function() bulk_quote_endpoint('nyse')

#' @export
#' @rdname fmp_quote
fmp_quote_nasdaq <- function() bulk_quote_endpoint('nasdaq')

#' @export
#' @rdname fmp_quote
fmp_quote_amex <- function() bulk_quote_endpoint('amex')

#' @export
#' @rdname fmp_quote
fmp_quote_tsx <- function() bulk_quote_endpoint('tsx')

#' @export
#' @rdname fmp_quote
fmp_quote_etfs <- function() bulk_quote_endpoint('etf')

#' @export
#' @rdname fmp_quote
fmp_quote_forex <- function() bulk_quote_endpoint('forex')

#' @export
#' @rdname fmp_quote
#'  @examples
#'
#' # real-time quote of every stock on nyse
#' df_crypto <- fmp_quote_cryptos()
fmp_quote_cryptos <- function() bulk_quote_endpoint('crypto')

#' @export
#' @rdname fmp_quote
fmp_quote_euronext <- function() bulk_quote_endpoint('euronext')

#' @export
#' @rdname fmp_quote
fmp_quote_mutual_funds <- function() bulk_quote_endpoint('mutual_fund')

#' @export
#' @rdname fmp_quote
fmp_quote_commodities <- function() bulk_quote_endpoint('commodity')

#' @export
#' @rdname fmp_quote
fmp_quote_indexes <- function() bulk_quote_endpoint('index')

