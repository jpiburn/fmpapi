#' Install a Financial Modeling Prep API Key in Your `.Renviron` File for Repeated Use
#'
#' @description This function will add your FMP API key to your `.Renviron` file
#'              so it can be called securely without being stored in your code.
#'              After you have installed your key, it can be called any time by
#'              typing `Sys.getenv('FMP_API_KEY')`.
#'
#' @param key `character`. The API key acquired from ['https://financialmodelingprep.com/developer/docs/pricing/'](https://financialmodelingprep.com/developer/docs/pricing/)
#' @param install `logical`. If `TRUE`, will install the key in your `.Renviron` file for use in future sessions.
#'        If `FALSE`, key is only loaded for the current R session and will not persist when a new session is loaded.
#'        This is potentially useful if you wish to access the API from a non-personal computer. Default is `TRUE`.
#' @param overwrite `logical`. If `TRUE`, overwrite any existing FMP_API_KEY that you already have in your `.Renviron` file.
#' @param fmpcloud_access logical. Is this key additionally activated on the ['fmpcloud.io'](https://fmpcloud.io) server?
#'        default is `FALSE`
#' @examples
#'
#' \dontrun{
#' key <- "my_api_key"
#'
#' fmp_api_key(key)
#'
#' # first time, either reload your environment or restart R
#' # reload environment with the following
#' readRenviron("~/.Renviron")
#'
#' # you can check it with:
#' Sys.getenv("FMP_API_KEY")
#' }
#' @export
fmp_api_key <- function(key, install = TRUE, overwrite = FALSE, fmpcloud_access = FALSE) {

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (file.exists(renv)) file.copy(renv, file.path(home, ".Renviron_backup"))

    if (!file.exists(renv)) {
      file.create(renv)
    } else {
      if (overwrite) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("FMP_API_KEY", oldenv), ]
        utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      } else {
        tv <- readLines(renv)
        if (any(grepl("FMP_API_KEY", tv))) {
          stop("A FMP_API_KEY already exists. You can overwrite it with the argument `overwrite = TRUE`", call. = FALSE)
        }
      }
    }

    key_text <- glue::glue("FMP_API_KEY='{key}'")
    fmpcloud_access_text <- glue::glue("FMP_CLOUD_ACCESS={fmpcloud_access}")

    write(key_text, renv, sep = "\n", append = TRUE)
    write(fmpcloud_access_text, renv, sep = "\n", append = TRUE)

    key_message <- glue::glue("Your API key has been stored in your .Renviron file and can be accessed by `Sys.getenv('FMP_API_KEY')`",
                              "To use now, restart R or run `readRenviron('~/.Renviron')`",
                              .sep = "\n"
                              )
    message(key_message)
    return(key)

  } else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(FMP_API_KEY = key)
    Sys.setenv(FMP_CLOUD_ACCESS = fmpcloud_access)
  }

}


#' @noRd
is_char_not_symbol <- function(x) {

  out <- FALSE

  if (is.character(x) && x != "symbol") out <- TRUE

  out
  }
