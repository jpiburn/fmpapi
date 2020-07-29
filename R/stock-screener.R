#' Company Stock Screener
#'
#'
#' @param marketCapMoreThan `numeric`. Default is `NULL`
#' @param marketCapLowerThan `numeric`. Default is `NULL`
#' @param betaMoreThan `numeric`. Default is `NULL`
#' @param betaLowerThan `numeric`. Default is `NULL`
#' @param volumeMoreThan `numeric`. Default is `NULL`
#' @param volumeLowerThan `numeric`. Default is `NULL`
#' @param dividendMoreThan `numeric`. Default is `NULL`
#' @param dividendLowerThan `numeric`. Default is `NULL`
#' @param sector `character`. See Details for avialable values
#' @param exchange `character`. See Details for avialable values
#' @param limit  `numeric`. Default is `NULL`
#'
#' @details
#' `sector` paramater can be anyone of the following:
#' * `"Consumer Cyclical"`
#' * `"Energy"`
#' * `"Technology"`
#' * `"Industrials"`
#' * `"Financial Services"`
#' * `"Basic Materials"`
#' * `"Communication Services"`
#' * `"Consumer Defensive"`
#' * `"Healthcare"`
#' * `"Real Estate"`
#' * `"Utilities"`
#' * `"Industrial Goods"`
#' * `"Financial"`
#' * `"Services"`
#' * `"Conglomerates"`
#'
#' `exchange` paramater can be anyone of the following:
#' * `"nyse"`
#' * `"nasdaq"`
#' * `"amex"`
#' * `"euronext"`
#' * `"tsx"`
#' * `"etf"`
#' * `"mutual_fund"`
#'
#' @return a [tibble][tibble::tibble-package] of stocks matching criteria
#'
#' @examples
#' library(fmp)
#'
#' # small cap, high beta tech stocks
#' d <- fmp_screen_stocks(marketCapLowerThan = 1e9, betaMoreThan = 2, sector = 'Technology')
#'
#' # mid cap healthcare stocks listed on the nasdaq
#' d <- fmp_screen_stocks(marketCapMoreThan = 1e9, marketCapLowerThan = 1e10,
#' sector = 'Healthcare', exchange = 'nasdaq')
#'
#' @export
fmp_screen_stocks <- function(
  marketCapMoreThan = NULL,
  marketCapLowerThan = NULL,
  betaMoreThan = NULL,
  betaLowerThan = NULL,
  volumeMoreThan = NULL,
  volumeLowerThan = NULL,
  dividendMoreThan = NULL,
  dividendLowerThan = NULL,
  sector = NULL,
  exchange = NULL,
  limit = NULL
  ) {
  query_list <- as.list(environment())

  # convert these numerics to integers to support values like 1e9
  int_fields <- c('marketCapMoreThan', 'marketCapLowerThan', 'volumeMoreThan', 'volumeLowerThan')
  for (int_field in int_fields) {
    field_value <- query_list[[int_field]]
    if (!is.null(field_value)) query_list[[int_field]] <- format(field_value, scientific = FALSE)
  }

  endpoint <- "stock-screener"

  request_urls <- build_request_urls(NULL, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}
