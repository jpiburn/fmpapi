#' Market Sector Performance
#'
#' @param historical `logical`. If `TRUE` return historical values. If `FALSE`
#'        return current. Default is `FALSE`
#'
#' @export
fmp_sector_performance <- function(historical = FALSE) {
  endpoint <- 'sectors-performance'
  if (historical) endpoint <- paste0('historical-', endpoint)

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


#' Most Active Stocks
#'
#' @export
fmp_market_actives <- function() {
  endpoint = 'actives'

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Largest Gaining Stocks
#'
#' @export
fmp_market_gainers <- function() {
  endpoint = 'gainers'

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Largest Losing Stocks
#'
#' @export
fmp_market_losers <- function() {
  endpoint = 'losers'

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' Market Hours and Holidays
#'
#' @export
fmp_market_hours <- function() {
  endpoint = 'market-hours'

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}
