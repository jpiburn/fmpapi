#' Retrieve Real-Time Quotes of Selected Securities and Cryptocurrencies
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_quote(my_stocks)
#'}
#'
#' @export
fmp_quote <- function(symbol) {
  endpoint <- "quote"

  symbol <- paste0(symbol, collapse = ",")

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

#' Retrieve Real-Time Quotes of Various Securities and Cryptocurrencies
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#'
#' @name fmpQuote
NULL

#' @rdname fmpQuote
#' @export
fmp_quote_nyse <- function() bulk_quote_endpoint('nyse')

#' @rdname fmpQuote
#' @export
fmp_quote_nasdaq <- function() bulk_quote_endpoint('nasdaq')

#' @rdname fmpQuote
#' @export
fmp_quote_amex <- function() bulk_quote_endpoint('amex')

#' @rdname fmpQuote
#' @export
fmp_quote_tsx <- function() bulk_quote_endpoint('tsx')

#' @rdname fmpQuote
#' @export
fmp_quote_etfs <- function() bulk_quote_endpoint('etf')

#' @rdname fmpQuote
#' @export
fmp_quote_forex <- function() bulk_quote_endpoint('forex')

#' @rdname fmpQuote
#' @export
fmp_quote_cryptos <- function() bulk_quote_endpoint('crypto')

#' @rdname fmpQuote
#' @export
fmp_quote_euronext <- function() bulk_quote_endpoint('euronext')

#' @rdname fmpQuote
#' @export
fmp_quote_mutual_funds <- function() bulk_quote_endpoint('mutual_fund')

#' @rdname fmpQuote
#' @export
fmp_quote_commodities <- function() bulk_quote_endpoint('commodity')

#' @rdname fmpQuote
#' @export
fmp_quote_indexes <- function() bulk_quote_endpoint('index')

