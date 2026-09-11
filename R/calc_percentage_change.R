#' Calculate percentage change
#'
#' Calculates percentage change between a new value and an old value for
#' rows with value type code 9.
#'
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param new_value_col Name of the column containing the new value.
#'   Defaults to `"numerator"`.
#' @param old_value_col Name of the column containing the old, previous, or
#'   baseline value. Defaults to `"denominator"`.
#' @param value_multiplier_col Name of the column containing the multiplier
#'   applied to the percentage change. Defaults to `"value_multiplier"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals are required. Defaults to `FALSE`. Confidence intervals are not
#'   currently implemented.
#'
#' @return A data frame containing rows with value type code 9, with `value`
#'   containing the calculated percentage change. `lowercl` and `uppercl`
#'   are set to `NA`.
#'
#' @details
#' Percentage change is calculated as:
#' `(new value - old value) / old value * value multiplier`.
#'
#' If the old value is zero or missing, `value` is returned as `NA`.
#'
#' @export
calc_percentage_change <- function(df, 
                                   value_type_code_col = "value_type_code", 
                                   new_value_col = "numerator",
                                   old_value_col = "denominator",
                                   value_multiplier_col = "value_multiplier",
                                   confidence_intervals_required = FALSE){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(value_type_code_col, new_value_col, old_value_col, value_multiplier_col)
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if(length(missing_cols) > 0){
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if(!is.logical(confidence_intervals_required) || 
     length(confidence_intervals_required) != 1 || 
     is.na(confidence_intervals_required) # Since NA is logical and has length 1, it needs to be explicitly excluded
  ){
    
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  if(confidence_intervals_required){
    stop(
      "Confidence intervals for percentage change have not yet been implemented.",
      call. = FALSE
    )
  }
  
  result <- df |> 
    dplyr::filter(
      .data[[value_type_code_col]] == 9L
    ) |> 
    dplyr::mutate(
      value = dplyr::if_else(
        .data[[old_value_col]] == 0 | is.na(.data[[old_value_col]]),
        NA_real_, # ignore 0/NA denominator as sometimes there are no plan/previous/baseline values
        (.data[[new_value_col]] - .data[[old_value_col]]) / 
          .data[[old_value_col]] * 
          .data[[value_multiplier_col]]
      ),
      lowercl =  NA_real_, 
      uppercl = NA_real_ 
    )
  
  result
}