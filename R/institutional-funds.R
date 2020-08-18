#' Form 13-F Statements
#'
#' Form 13-F statements provides position-level report of all institutional
#' investment managers with more than $100m in assets under management
#'
#' @param cik `character`. A vector of Central Index Keys (CIK)
#' @param year `numeric`. 4 digit year, e.g. `2020`. If `NULL`, the current year
#'  will be taken from [Sys.Date()]. Default is `NULL`
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @seealso [fmp_list_ciks()]
#' @examples
#'
#' berkshire_cik <- '0001067983'
#' \donttest{
#' d <- fmp_13f(berkshire_cik)
#' }
#'
#' \donttest{
#' # 13-F forms from 2018
#' d_18 <- fmp_13f(berkshire_cik, year = 2018)
#' }
#'
fmp_13f <- function(cik, year = NULL) {
  endpoint <- "form-thirteen"

  year <- ifelse(is.null(year), substr(Sys.Date(), start = 1, stop = 4), year)
  query_list <- list(year = year)

  # TODO: Currently can only pass one year at a time.
  #       need to determine best way to vectorize
  request_urls <- build_request_urls(cik, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}



#' SEC RSS Feed
#'
#' Retrieve the most recent financial statements filed to the SEC
#'
#' @return a [tibble][tibble::tibble-package] of filing information
#' @export
fmp_sec_filings <- function() {
  endpoint <- "rss_feed"

  request_urls <- build_request_urls(NULL, endpoint = endpoint)
  d <- get_request_content(request_urls)

  d
}
