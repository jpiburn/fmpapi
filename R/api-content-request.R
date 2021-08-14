
#' @noRd
get_request_content <- function(request_urls, ...) {
  arguments <- list(...)

  d <- purrr::map_dfr(request_urls, ~ request_url_content(.x, arguments = arguments))
  d <- format_request_content(d, arguments = arguments)

  d
}


#' @noRd
request_url_content <- function(request_url, arguments) {

  ua <- httr::user_agent("https://github.com/jpiburn/fmpapi")
  get_return <- httr::GET(request_url, ua)

  if (httr::http_error(get_return)) {

    error_status <- httr::http_status(get_return)
    error_message <- glue::glue(
      'API request failed for failed and returned the following information.',
      'request_url: {request_url}',
      'message: {error_status$message}',
      'category: {error_status$category}',
      'reason: {error_status$reason}',
      .sep = "\n")

    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(get_return) != "application/json") {

    error_status <- httr::http_status(get_return)
    error_message <- glue::glue(
      'API request executed successfully, but did not return json as expected.',
      'request_url: {request_url}',
      'content_type: {httr::http_type(get_return)}',
      'message: {error_status$message}',
      'category: {error_status$category}',
      'reason: {error_status$reason}',
      .sep = "\n")

    stop(error_message, call. = FALSE)
  }

  return_json <- httr::content(get_return, as = "text")
  d <- jsonlite::fromJSON(return_json)

  if (length(arguments$endpoint) != 0) {
    if (arguments$endpoint[[1]] == 'historical-price-full') {
      if (all(c('symbol', 'historical') %in% names(d))) {
        symbol <- d$symbol
        d <- tibble::as_tibble(d$historical)
        d <- tibble::add_column(d, symbol, .before = 1)
      }
    }
  }

  d
}

