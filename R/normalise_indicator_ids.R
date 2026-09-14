#' Normalise indicator IDs
#'
#' Standardises indicator ID input into a consistent format for use across
#' metric engine functions. The function supports requests to process all
#' indicators, numeric or character vectors, and single comma-separated
#' character strings.
#'
#' @param indicator_ids Indicator IDs to normalise. Accepted inputs include:
#'
#'   \itemize{
#'     \item `NULL` to indicate that all indicators should be processed.
#'     \item `"All"`, `"all"`, or `"*"` to indicate that all indicators
#'       should be processed.
#'     \item A numeric vector, such as `c(10, 11, 12)`.
#'     \item A character vector, such as `c("10", "11", "12")`.
#'     \item A single comma-separated character string, such as
#'       `"10, 11, 12"`.
#'   }
#'
#' @return `NULL` when all indicators should be processed. Otherwise, returns
#'   a vector containing the normalised indicator IDs with surrounding
#'   whitespace, missing values, blank values, and duplicates removed.
#'
#'   Values that can be converted to numeric are converted where possible.
#'   Values that cannot be converted to numeric are retained as character
#'   values.
#'
#' @details
#' The function first validates that `indicator_ids` is either `NULL`,
#' numeric, or character.
#'
#' Character inputs have leading and trailing whitespace removed using
#' `trimws()`.
#'
#' A single value of `"All"`, `"all"`, or `"*"` is interpreted as a request
#' to process all indicators and therefore returns `NULL`.
#'
#' If a single comma-separated character string is supplied, it is split into
#' individual values before further processing.
#'
#' Missing and blank values are removed. The function then attempts to convert
#' each remaining value to numeric. Values that can be converted are represented
#' numerically where possible, while non-numeric values are retained as
#' character values.
#'
#' Duplicate indicator IDs are removed before the final vector is returned.
#'
#' @examples
#' normalise_indicator_ids(NULL)
#'
#' normalise_indicator_ids("All")
#'
#' normalise_indicator_ids(c(10, 11, 12))
#'
#' normalise_indicator_ids(c("10", "11", "12"))
#'
#' normalise_indicator_ids("10, 11, 12")
#'
#' normalise_indicator_ids(c("10", "ABC", "12"))
#'
#' @export
normalise_indicator_ids <- function(indicator_ids) {
  
  # NULL means process all indicators
  if (is.null(indicator_ids)) {
    return(NULL)
  }
  
  # Validate input type
  if (!is.character(indicator_ids) && !is.numeric(indicator_ids)) {
    stop(
      "`indicator_ids` must be NULL, \"All\", \"*\", or a numeric/character vector.",
      call. = FALSE
    )
  }
  
  # Remove surrounding whitespace from character inputs
  if (is.character(indicator_ids)) {
    indicator_ids <- trimws(indicator_ids)
  }
  
  # Treat "All" / "all" / "*" as no filter
  if (
    is.character(indicator_ids) &&
    length(indicator_ids) == 1 &&
    tolower(indicator_ids) %in% c("all", "*")
  ) {
    return(NULL)
  }
  
  # Split a single comma-separated string
  if (
    is.character(indicator_ids) &&
    length(indicator_ids) == 1 &&
    grepl(",", indicator_ids)
  ) {
    indicator_ids <- trimws(
      unlist(
        strsplit(
          indicator_ids,
          ",",
          fixed = TRUE
        )
      )
    )
  }
  
  # Remove missing and blank values
  indicator_ids <- indicator_ids[
    !is.na(indicator_ids) &
      indicator_ids != ""
  ]
  
  # Coerce numerics when possible; keep characters that aren't numeric
  as_num <- suppressWarnings(
    as.numeric(indicator_ids)
  )
  
  out <- ifelse(
    !is.na(as_num),
    as_num,
    indicator_ids
  )
  
  # Remove duplicates
  unique(out)
}





