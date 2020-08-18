#' List Available Securities
#'
#' Group of functions for listing available securities
#'
#' Group of functions Details paragraph.
#'
#'
#' @section After Arguments and Value sections:
#' Despite its location, this actually comes after the Arguments and Value sections.
#' Also, don't need to use null, could annotate first function, and then
#' using function name as the groupBy name is more intuitive.
#'
#'
#' @return a [tibble][tibble::tibble-package] of quotes of requested securities
#'
#' @examples
#'
#' \donttest{
#' library(fmp)
#'
#' # all available stocks
#' df_stocks <- fmp_list_stocks()
#'
#' df_etfs <- fmp_list_etfs()
#' }
#' @name fmp_list
NULL

#' @noRd
bulk_list_endpoint <- function(endpoint) {
  endpoint <- c('symbol', endpoint)

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' @rdname fmp_list
#' @export
fmp_list_nyse <- function() bulk_list_endpoint('nyse')

#' @rdname fmp_list
#' @export
fmp_list_nasdaq <- function() bulk_list_endpoint('nasdaq')

#' @rdname fmp_list
#' @export
fmp_list_amex <- function() bulk_list_endpoint('amex')

#' @rdname fmp_list
#' @export
fmp_list_tsx <- function() bulk_list_endpoint('available-tsx')

#' @rdname fmp_list
#' @export
fmp_list_etfs <- function() bulk_list_endpoint('available-etfs')

#' @rdname fmp_list
#' @export
fmp_list_forex <- function() bulk_list_endpoint('available-forex-currency-pairs')

#' @rdname fmp_list
#' @export
fmp_list_cryptos <- function() bulk_list_endpoint('available-cryptocurrencies')

#' @rdname fmp_list
#' @export
fmp_list_euronext <- function() bulk_list_endpoint('available-euronext')

#' @rdname fmp_list
#' @export
fmp_list_mutual_funds <- function() bulk_list_endpoint('available-mutual-funds')

#' @rdname fmp_list
#' @export
fmp_list_commodities <- function() bulk_list_endpoint('available-commodities')

#' @rdname fmp_list
#' @export
fmp_list_indexes <- function() bulk_list_endpoint('available-indexes')

#' @rdname fmp_list
#' @export
fmp_list_ciks <- function() {
  endpoint <- "cik_list"

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' @rdname fmp_list
#' @export
fmp_list_stocks <- function() {
  endpoint <- "stock"

  request_urls <- build_request_urls('list', endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


