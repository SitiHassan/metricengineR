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