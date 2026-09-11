#' Calculate counts
#'
#' Assigns count values to `value` for rows with value type code 1.
#'
#' @param df A data frame containing count values.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param count_col Name of the column containing the count values.
#'   Defaults to `"numerator"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be calculated. Defaults to `FALSE`.
#'
#' @return A data frame containing rows with value type code 1, with the count
#'   copied to `value`. `lowercl` and `uppercl` are set to `NA`.
#'
#' @details
#' Confidence intervals for counts are not currently implemented. If
#' `confidence_intervals_required = TRUE`, the function returns an error.
#'
#' @export
calc_count <- function(df,
                       value_type_code_col = "value_type_code",
                       count_col = "numerator",
                       confidence_intervals_required = FALSE){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(value_type_code_col, count_col)
  
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
      "Confidence intervals for counts have not yet been implemented.",
      call. = FALSE
    )
  }
  
  result <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 1L
      ) |>  
    dplyr::mutate(
      value = .data[[count_col]],
      lowercl = NA_real_,
      uppercl = NA_real_
    )
  
  result
  
}