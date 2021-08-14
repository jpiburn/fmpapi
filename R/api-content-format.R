
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

