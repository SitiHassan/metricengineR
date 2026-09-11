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
      indicator_id     = as.integer(.data$indicator_id),
      start_date       = as.Date(.data$start_date),
      end_date         = as.Date(.data$end_date),
      numerator        = as.numeric(.data$numerator),
      denominator      = as.numeric(.data$denominator),
      indicator_value  = as.numeric(.data$indicator_value),
      lower_ci95       = as.numeric(.data$lower_ci95),
      upper_ci95       = as.numeric(.data$upper_ci95),
      imd_code         = as.integer(.data$imd_code),
      aggregation_id   = as.integer(.data$aggregation_id),
      age_group_code   = as.integer(.data$age_group_code),
      sex_code         = as.integer(.data$sex_code),
      ethnicity_code   = as.integer(.data$ethnicity_code),
      creation_date    = as.POSIXct(.data$creation_date),
      value_type_code  = as.integer(.data$value_type_code),
      source_code      = as.integer(.data$source_code),
      time_period_type = as.character(.data$time_period_type),
      combination_id   = as.integer(.data$combination_id)
    )
}