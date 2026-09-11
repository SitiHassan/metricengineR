#' Calculate differences between two values
#'
#' Calculates the difference between a minuend and subtrahend for
#' difference-based value types.
#'
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param minuend_col Name of the column containing the value to subtract from.
#'   Defaults to `"numerator"`.
#' @param subtrahend_col Name of the column containing the value to subtract.
#'   Defaults to `"denominator"`.
#'
#' @return A data frame containing rows with value type codes 10 or 11, with
#'   `value` calculated as minuend minus subtrahend. `lowercl` and `uppercl`
#'   are set to `NA`.
#'
#' @details
#' Value type code 10 represents percentage point difference and value type
#' code 11 represents crude difference.
#'
#' The function checks that the input is a data frame and that the required
#' columns are present before calculation.
#'
#' @export
calc_difference <- function(df, 
                            value_type_code_col = "value_type_code", 
                            minuend_col = "numerator",
                            subtrahend_col = "denominator"){
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(value_type_code_col, minuend_col, subtrahend_col)
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if(length(missing_cols) > 0){
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  result <- df |> 
    dplyr::filter(
      .data[[value_type_code_col]] %in% c(10L, 11L) # 10 = percentage point difference; 11 = crude difference
      ) |> 
    dplyr::mutate(
      value = .data[[minuend_col]] - .data[[subtrahend_col]],
      lowercl = NA_real_,
      uppercl = NA_real_
    )
  
  result
}



