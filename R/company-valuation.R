#' Company Valuation
#'
#' Group of functions Description section
#'
#' Group of functions Details paragraph.
#'
#' @section After Arguments and Value sections:
#' Despite its location, this actually comes after the Arguments and Value sections.
#' Also, don't need to use null, could annotate first function, and then
#' using function name as the groupBy name is more intuitive.
#'
#' @param symbol `character`. A vector of stock symbols.
#' @param historical `logical`. If `TRUE` return historical dcf values. If `FALSE`
#'        return current estimate. Default is `FALSE`
#' @param quarterly `logical`. If `TRUE` return quarterly. If `FALSE`
#'        return annual. Default is `FALSE`
#'
#' @return a [tibble][tibble::tibble-package] of
#' @examples
#' my_stocks <- c('AAPL', 'GE')
#'
#' @name companyValuation
NULL


#' Company Profile
#'
#' @param symbol `character`. A vector of stock symbols.
#'
#' @return a [tibble][tibble::tibble-package] of relevant financial information
#'
#' @examples
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_profile(my_stocks)
#' @export
#' @family `Company Summaries`
fmp_profile <- function(symbol) {
  endpoint <- "profile"

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


#' Company Income Statement
#'
#' @inheritParams fmp_profile
#' @inherit fmp_profile return
#'
#' @param quarterly `logical`. If `TRUE` return quarterly. If `FALSE`
#'        return annual. Default is `FALSE`
#'
#' @examples
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_income(my_stocks, quarterly = TRUE)
#'
#' @export
#' @family `Company Summaries`
fmp_income <- function(symbol, quarterly = FALSE) {
  endpoint <- "income-statement"
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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_balance_sheet(my_stocks, quarterly = TRUE)
#'
#' @export
#' @family `Company Summaries`
fmp_balance_sheet <- function(symbol, quarterly = FALSE) {
  endpoint <- "balance-sheet-statement"
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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_cash_flow(my_stocks, quarterly = TRUE)
#'
#' @export
#' @family `Company Summaries`
fmp_cash_flow <- function(symbol, quarterly = FALSE) {
  endpoint <- "cash-flow-statement"
  query_list <- list(period = NULL)
  if (quarterly) query_list$period <- "quarter"

  request_urls <- build_request_urls(symbol, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}


#' Company Financial Ratios
#'
#' @inheritParams fmp_income
#' @inherit fmp_profile return
#'
#' @examples
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_ratios(my_stocks, quarterly = TRUE)
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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_enterprise_value(my_stocks, quarterly = TRUE)
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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_key_metrics(my_stocks, quarterly = TRUE)
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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_financial_growth(my_stocks, quarterly = TRUE)
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
#'
#' @examples
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_rating(my_stocks, quarterly = TRUE)
#'
#' @export
#' @family `Company Summaries`
fmp_rating <- function(symbol) {
  endpoint <- "rating"

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
#' my_stocks <- c('AAPL', 'GE')
#' d <- fmp_dcf(my_stocks, historical = TRUE, quarterly = TRUE)
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

#' List Available Stock Symbols
#'
#' @return [tibble::tbl_df]
#' @export
#'
#' @examples
#' d <- fmp_list_stocks()
fmp_list_stocks <- function() {
  endpoint <- "stock"

  request_urls <- build_request_urls('list', endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

# fmp_earnings_calendar <- function(symbol = NULL) {
#   endpoint <- "earning_calendar"
#
#   if (is.null(symbol)) {
#     request_urls <- build_request_urls(symbol = NULL, endpoint = endpoint)
#     d <- get_request_content(request_urls)
#   }
#   else {
#     endpoint <- c("historical", endpoint)
#     request_urls <- build_request_urls(symbol, endpoint = endpoint)
#     d <- get_request_content(request_urls)
#   }
#
#   d
# }

#' @rdname companyValuation
#'
#' @export
#' @examples
#' d <- fmp_earnings_calendar()
fmp_earnings_calendar <- function() {
  endpoint <- "earning_calendar"

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}

#' @rdname companyValuation
#'
#' @note Currently `fmp_earnings()` requirest increased API access
#'
#' @export
#' @examples
fmp_earnings <- function(symbol) {
  endpoint <- c("historical", "earning_calendar")

  request_urls <- build_request_urls(symbol, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}


fmp_stock_price <- function() {}

fmp_list_indexes <- function() {}
