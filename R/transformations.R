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


#' Clean and standardise metric data types
#'
#' Converts columns within a metric data frame to the standard data types
#' expected by the metric engine.
#'
#' @param data A data frame containing metric data to be standardised.
#'   The data frame must contain the following columns:
#'
#'   \itemize{
#'     \item `indicator_id`
#'     \item `start_date`
#'     \item `end_date`
#'     \item `numerator`
#'     \item `denominator`
#'     \item `indicator_value`
#'     \item `lower_ci95`
#'     \item `upper_ci95`
#'     \item `imd_code`
#'     \item `aggregation_id`
#'     \item `age_group_code`
#'     \item `sex_code`
#'     \item `ethnicity_code`
#'     \item `creation_date`
#'     \item `value_type_code`
#'     \item `source_code`
#'     \item `time_period_type`
#'     \item `combination_id`
#'   }
#'
#' @return The input data frame with the specified columns converted to their
#'   standard metric engine data types.
#'
#' @details
#' The function applies a consistent data type schema to metric data before
#' further processing or combining outputs from different calculation
#' functions.
#'
#' The following conversions are applied:
#'
#' \itemize{
#'   \item Identifier and coded fields are converted to integer.
#'   \item `start_date` and `end_date` are converted to `Date`.
#'   \item Numerators, denominators, indicator values, and confidence intervals
#'     are converted to numeric.
#'   \item `creation_date` is converted to `POSIXct`.
#'   \item `time_period_type` is converted to character.
#' }
#'
#' Before conversion, the function checks that `data` is a data frame and that
#' all required columns are present. If required columns are missing, execution
#' stops and the missing column names are reported.
#'
#' Values that cannot be converted to the required data type may be converted
#' to `NA` and may generate a warning from the relevant base R conversion
#' function.
#'
#' @section External dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `dplyr::mutate()` - applies the required data type conversions.
#' }
#'
#' @export
clean_data_types <- function(data){
  
  # Validate input
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "numerator",
    "denominator",
    "indicator_value",
    "lower_ci95",
    "upper_ci95",
    "imd_code",
    "aggregation_id",
    "age_group_code",
    "sex_code",
    "ethnicity_code",
    "creation_date",
    "value_type_code",
    "source_code",
    "time_period_type",
    "combination_id"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  data |>
    dplyr::mutate(
      indicator_id     = as.integer(indicator_id),
      start_date       = as.Date(start_date),
      end_date         = as.Date(end_date),
      numerator        = as.numeric(numerator),
      denominator      = as.numeric(denominator),
      indicator_value  = as.numeric(indicator_value),
      lower_ci95       = as.numeric(lower_ci95),
      upper_ci95       = as.numeric(upper_ci95),
      imd_code         = as.integer(imd_code),
      aggregation_id   = as.integer(aggregation_id),
      age_group_code   = as.integer(age_group_code),
      sex_code         = as.integer(sex_code),
      ethnicity_code   = as.integer(ethnicity_code),
      creation_date    = as.POSIXct(creation_date),
      value_type_code  = as.integer(value_type_code),
      source_code      = as.integer(source_code),
      time_period_type = as.character(time_period_type),
      combination_id   = as.integer(combination_id)
    )
}