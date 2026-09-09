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


#' Tidy metric calculation output
#'
#' Standardises the structure of metric calculation outputs so that results
#' from different calculation functions can be combined using a consistent
#' set of columns and column names.
#'
#' @param data A data frame containing metric calculation results.
#'
#' @return A data frame containing the standard metric engine output columns
#'   in a consistent order. Missing standard columns are added and populated
#'   with `NA`.
#'
#' @details
#' The function validates that `data` is a data frame and then standardises
#' alternative calculation output column names to the metric engine schema.
#'
#' The following alternative column names are handled:
#'
#' \itemize{
#'   \item `value` is used to populate `indicator_value` when
#'     `indicator_value` is not already present.
#'   \item `lowercl` is used to populate `lower_ci95` when
#'     `lower_ci95` is not already present.
#'   \item `uppercl` is used to populate `upper_ci95` when
#'     `upper_ci95` is not already present.
#' }
#'
#' If both the standard column and its alternative column are present, the
#' existing standard column is retained.
#'
#' Any standard output columns that are not present in the input data are
#' created and populated with `NA`.
#'
#' The final output is restricted to the standard metric engine columns and
#' returned in the following order:
#'
#' \itemize{
#'   \item `indicator_id`
#'   \item `start_date`
#'   \item `end_date`
#'   \item `numerator`
#'   \item `denominator`
#'   \item `indicator_value`
#'   \item `lower_ci95`
#'   \item `upper_ci95`
#'   \item `imd_code`
#'   \item `aggregation_id`
#'   \item `age_group_code`
#'   \item `sex_code`
#'   \item `ethnicity_code`
#'   \item `creation_date`
#'   \item `value_type_code`
#'   \item `source_code`
#'   \item `time_period_type`
#'   \item `combination_id`
#' }
#'
#' Columns in the input data that are not part of the standard output schema
#' are removed from the returned data frame.
#'
#' @section External dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `dplyr::select()` - selects and orders the standard output columns.
#'   \item `dplyr::all_of()` - ensures that all standard output columns are
#'     selected in the defined order.
#' }
#'
#' @export
tidy_output <- function(data) {
  
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }
  
  TIDY_COLS <- c(
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
  
  out <- data
  
  if (!"indicator_value" %in% names(out)) {
    
    if ("value" %in% names(out)) {
      out$indicator_value <- out$value
    } else {
      out$indicator_value <- NA_real_
    }
  }
  
  if (!"lower_ci95" %in% names(out)) {
    
    if ("lowercl" %in% names(out)) {
      out$lower_ci95 <- out$lowercl
    } else {
      out$lower_ci95 <- NA_real_
    }
  }
  
  if (!"upper_ci95" %in% names(out)) {
    
    if ("uppercl" %in% names(out)) {
      out$upper_ci95 <- out$uppercl
    } else {
      out$upper_ci95 <- NA_real_
    }
  }
  
  missing_cols <- setdiff(
    TIDY_COLS,
    names(out)
  )
  
  out[missing_cols] <- NA
  
  out |>
    dplyr::select(
      dplyr::all_of(TIDY_COLS)
    )
}