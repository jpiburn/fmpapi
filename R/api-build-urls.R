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

