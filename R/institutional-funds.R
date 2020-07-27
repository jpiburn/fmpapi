#' Institutional Fund
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

#' @param year `numeric`. 4 digit year, e.g. `2020`
#'
#' @return a [tibble][tibble::tibble-package]
#' @name institutionalFund
NULL


#' @rdname institutionalFund
#'
#' @param cik `character`. A vector of Central Index Keys (CIK)
#' @export
#' @examples
#'
#' berkshire_cik <- '0001067983'
#' d <- fmp_13f(berkshire_cik, year = 2020)
fmp_13f <- function(cik, year) {
  endpoint <- "form-thirteen"

  query_list <- list(year = year)

  # TODO: Currently can only pass one year at a time.
  #       need to determine best way to vectorize
  request_urls <- build_request_urls(cik, endpoint = endpoint, query_list = query_list)
  d <- get_request_content(request_urls)

  d
}
