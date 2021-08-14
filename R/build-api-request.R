#' Currently requests are rate limited to 8 per
#' @noRd
get_base_url <- function(base_source = c('fmp', 'cloud', 'all')) {
  base_source <- match.arg(base_source)

  if (base_source == 'fmp') base_url <- 'https://financialmodelingprep.com'
  if (base_source == 'cloud') base_url <- 'https://fmpcloud.io'
  if (base_source == 'all') base_url <-  c('https://financialmodelingprep.com', 'https://fmpcloud.io')

  base_url
}

#' @noRd
build_request_urls <- function(path_params, api_version = 'v3', endpoint, query_list = NULL, base_url = NULL) {

  if (Sys.getenv('FMP_API_KEY') != '')  {

    apikey <- Sys.getenv('FMP_API_KEY')
    cloud_access <- as.logical(Sys.getenv('FMP_CLOUD_ACCESS'))
    if(is.na(cloud_access)) cloud_access <- FALSE

  } else {

    warning_message <- glue::glue(
      'An API Key is required to use the Financial Modeling Prep API and fmp package.',
      'No API Key was found and a limited use key of "demo" is being used.',
      'To obtain an API key please visit https://financialmodelingprep.com/developer/docs/pricing/',
      "\nOnce obtained, you can call `fmp::fmp_api_key('my_api_key')` to save your key to .Renviron file.",
      "\nWhen first installed, you will need to reload your .Renviron file by either restarting R or running `readRenviron('~/.Renviron')`"
    )

    warning(warning_message)
    apikey <- 'demo'
  }

  url_path_stem <- paste0(c('api', api_version, endpoint), collapse = "/")
  if (!is.null(path_params)) url_path_stem <- paste(url_path_stem, path_params, sep = "/")

  null_queries <- sapply(query_list, is.null)

  if (all(null_queries)) url_query_stem <- paste0("apikey=", apikey)
  else{
    query_list <- query_list[!null_queries]
    url_df <- expand.grid(query_list)
    query_names <- names(url_df)
    for (i in 1:ncol(url_df)) {
      url_df[ , query_names[i]] <- paste0(query_names[i], "=", url_df[ , query_names[i]])
    }
    url_df$apikey <- paste0("apikey=", apikey)
    url_query_stem <- apply(url_df, 1, paste0, collapse = "&")
  }
  endpoint_urls_df <- tidyr::expand_grid(path = url_path_stem, query = url_query_stem)

  # determine base_urls -----
  if (is.null(base_url)) {
    if (cloud_access) base_url <- get_base_url('all')
    else base_url <- get_base_url('fmp')
  }

  n_params <- nrow(endpoint_urls_df)
  n_urls <- length(base_url)
  url_base_stem <- rep(base_url, ceiling(n_params / n_urls))[1:n_params]
  endpoint_urls_df$base_url <- url_base_stem

  endpoint_urls_df$request_url <- glue::glue_data(endpoint_urls_df, '{base_url}/{path}?{query}')
  request_urls <- endpoint_urls_df$request_url

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


#' @noRd
format_request_content <- function(df, arguments) {

  if (length(arguments) != 0) { # any additional special case formatting

    # any endpoint specific formatting -----
    if (!is.null(arguments$endpoint)) {

      # dcf -----
      if (arguments$endpoint[[1]] == 'historical-discounted-cash-flow' && arguments$historical == TRUE)
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

  # the symbol for TrueCar is "TRUE" so need to skip the symbol col when parsing
  # which is probably a good idea anyways
  # this would work but where() is not an exported function from tidyselect
  # it might be in the next release... https://github.com/r-lib/tidyselect/issues/201
  # d <- dplyr::mutate(d, dplyr::across(tidyselect:::where(is.character) & !symbol, readr::parse_guess))

  # until where is exported just use this
  skip_cols <- c("symbol", "phone", "zip", "cik", "isin",
                 "cusip", "address", "city", "state", "website",
                 "country", "description", "ceo")
  col_types <- sapply(d, class)
  char_cols <- setdiff(names(col_types[col_types == "character"]), skip_cols)

  lgl_cols <- names(col_types[col_types == "logical"])
  lgl_cols <- setdiff(lgl_cols[!grepl("is_|default_image", lgl_cols)], skip_cols)

  d <- dplyr::mutate(d, dplyr::across(char_cols, readr::parse_guess))
  d <- dplyr::mutate(d, dplyr::across(lgl_cols, as.numeric))
  d <- dplyr::mutate_if(d, is.integer, as.numeric)

  # not sure why I put this here... keeping in case there was an actual reason and i forgot
  # d <- dplyr::mutate_if(d, is.logical, as.numeric)


  d
}

