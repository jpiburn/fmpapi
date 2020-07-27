#' @noRd
bulk_list_endpoint <- function(endpoint) {
  endpoint <- c('symbol', endpoint)

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


fmp_list_nyse <- function() bulk_list_endpoint('nyse')

fmp_list_nasdaq <- function() bulk_list_endpoint('nasdaq')

fmp_list_amex <- function() bulk_list_endpoint('amex')

fmp_list_tsx <- function() bulk_list_endpoint('available-tsx')

fmp_list_etfs <- function() bulk_list_endpoint('available-etfs')

fmp_list_forex <- function() bulk_list_endpoint('available-forex-currency-pairs')

fmp_list_cryptos <- function() bulk_list_endpoint('available-cryptocurrencies')

fmp_list_euronext <- function() bulk_list_endpoint('available-euronext')

fmp_list_mutual_funds <- function() bulk_list_endpoint('available-mutual-funds')

fmp_list_commodities <- function() bulk_list_endpoint('available-commodities')

fmp_list_indexes <- function() bulk_list_endpoint('available-indexes')


fmp_list_ciks <- function() {
  endpoint <- "cik_list"

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}



