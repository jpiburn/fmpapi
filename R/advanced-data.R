#' A Company's Free Float
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_shares_float(my_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_shares_float <- function(symbol) {
  endpoint <- "shares_float"

  query_list <-
    list(
      symbol = symbol
    )

  request_urls <- build_request_urls(NULL, api_version = 'v4', endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}


#' Institutional Holders
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_institutional_holdersmy_stocks)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_institutional_holders <- function(symbol) {
  endpoint <- "institutional-holder"
  stop("need to add symbol to each request before returning")
  request_urls <- build_request_urls(symbol, api_version = 'v3', endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}

