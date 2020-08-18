#' Company Income Statement
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#'
#' @param quarterly `logical`. If `TRUE` return quarterly. If `FALSE`
#'        return annual. Default is `FALSE`
#' @param as_reported `logical`. If `TRUE` return data formatted as reported. Default
#'        is `FALSE`
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_income(my_stocks, quarterly = TRUE)
#'
#' d <- fmp_income(my_stocks, quarterly = TRUE, as_reported = TRUE)
#' }
#' @export
#' @family `Company Summaries`
fmp_income <- function(symbol, quarterly = FALSE, as_reported = FALSE) {
  endpoint <- "income-statement"
  if (as_reported) endpoint <- add_as_reported(endpoint)

  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"


  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Balance Sheet
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_balance_sheet(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_balance_sheet <- function(symbol, quarterly = FALSE, as_reported = FALSE) {
  endpoint <- "balance-sheet-statement"
  if (as_reported) endpoint <- add_as_reported(endpoint)

  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Cash Flow
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_cash_flow(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_cash_flow <- function(symbol, quarterly = FALSE, as_reported = FALSE) {
  endpoint <- "cash-flow-statement"
  if (as_reported) endpoint <- add_as_reported(endpoint)

  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Full Financial Statement
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#'
#' @param quarterly `logical`. If `TRUE` return quarterly. If `FALSE`
#'        return annual. Default is `FALSE`
#'
#' @examples
#'
#' \donttest{
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_full_financial(my_stocks, quarterly = TRUE)
#' }
#'
#' @export
#' @family `Company Summaries`
fmp_full_financial <- function(symbol, quarterly = FALSE) {
  endpoint <- "financial-statement-full"
  endpoint <- add_as_reported(endpoint)

  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' @noRd
add_as_reported <- function(endpoint) paste0(endpoint, '-as-reported')
