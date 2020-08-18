#' @noRd
build_request_urls <- function(base_path, endpoint, query_list = NULL) {

  if (Sys.getenv('FMP_API_KEY') != '')  {

    apikey <- Sys.getenv('FMP_API_KEY')

  } else {

    stop_message <- glue::glue(
      'An API Key is required to use the Financial Modeling Prep API and fmp package.',
      'To obtain an API key please visit https://financialmodelingprep.com/developer/docs/pricing/',
      "\nOnce obtained, you can call `fmp::fmp_api_key('my_api_key')` to save your key to .Renviron file.",
      "\nWhen first installed, you will need to reload your .Renviron file by either restarting R or running `readRenviron('~/.Renviron')`"
      )

    stop(stop_message)
  }


  if (is.null(base_path)) {
    request_urls <- httr::modify_url('https://financialmodelingprep.com',
                                     path =  c('api', 'v3', endpoint),
                                     query = as.list(c(query_list, apikey = apikey))
                                     )
  }
  else {
  request_urls <- purrr::map_chr(base_path,
                 ~ httr::modify_url('https://financialmodelingprep.com',
                                    path =  c('api', 'v3', endpoint, .x),
                                    query = as.list(c(query_list, apikey = apikey))
                                    )
                                 )
  }

  request_urls
}

#' @noRd
get_request_content <- function(request_urls, ...) {
  arguments <- list(...)

  d <- purrr::map_dfr(request_urls, ~ request_url_content(.x, arguments = arguments))
  d <- format_request_content(d, arguments = arguments)

  d
}


#' @noRd
request_url_content <- function(request_url, arguments) {

  ua <- httr::user_agent("https://github.com/jpiburn/fmp")
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
      symbol <- d$symbol
      d <- tibble::as_tibble(d$historical)
      d <- tibble::add_column(d, symbol, .before = 1)
    }
  }

  d
}


#' @noRd
format_request_content <- function(df, arguments) {

  if (length(arguments) != 0) { # any additional special case formatting

    # any endpoint specific formatting -----
    if (!is.null(arguments$endpoint)) {

      # dcf -----
      if (arguments$endpoint == 'historical-discounted-cash-flow' && arguments$historical == TRUE)
        df <- tidyr::unnest(df, cols = tidyselect::contains("historicalDCF"))

      # historical-price-full
      if (arguments$endpoint[[1]] == 'historical-price-full') {

      }
      # currently no others -----
    }

  }

  # formatting for all content -----
  d <- janitor::clean_names(df)
  d <- tibble::as_tibble(d)

  d <- dplyr::mutate_if(d, is.character, readr::parse_guess)
  d <- dplyr::mutate_if(d, is.integer, as.numeric)
  d <- dplyr::mutate_if(d, is.logical, as.numeric)


  d
}
