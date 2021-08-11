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

  request_urls <- build_request_urls(symbol, api_version = 'v4', endpoint = endpoint)
  d <- get_request_content(request_urls, endpoint = endpoint)

  d
}
