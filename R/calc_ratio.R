#' Calculate ratios
#'
#' Calculates ratio values for rows with value type code 7.
#'
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param numerator_col Name of the numerator column.
#'   Defaults to `"numerator"`.
#' @param denominator_col Name of the denominator column.
#'   Defaults to `"denominator"`.
#' @param value_multiplier_col Name of the column containing the value multiplier.
#'   Defaults to `"value_multiplier"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be calculated. Defaults to `FALSE`.
#'
#' @return A data frame containing ratio rows with calculated `value`,
#'   `lowercl`, and `uppercl` columns.
#'
#' @details
#' Ratios are calculated as numerator divided by denominator multiplied by the
#' value multiplier.
#'
#' Rows with missing values or a denominator of zero are retained with
#' calculated values set to `NA`.
#'
#' Confidence intervals for ratios are not currently implemented.
#'
#' @export
calc_ratio <- function(df,
                       value_type_code_col = "value_type_code",
                       numerator_col = "numerator",
                       denominator_col = "denominator",
                       value_multiplier_col = "value_multiplier",
                       confidence_intervals_required = FALSE){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    value_type_code_col,
    numerator_col,
    denominator_col,
    value_multiplier_col
  )
  
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
     is.na(confidence_intervals_required)){
    
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # Filter ratio rows
  result <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 7L
    )
  
  # Define rows eligible for calculation
  eligible <- !is.na(result[[numerator_col]]) &
    !is.na(result[[denominator_col]]) &
    result[[denominator_col]] != 0 &
    !is.na(result[[value_multiplier_col]])
  
  # Initialise calculated columns
  result$value <- rep(NA_real_, nrow(result))
  result$lowercl <- rep(NA_real_, nrow(result))
  result$uppercl <- rep(NA_real_, nrow(result))
  
  if(confidence_intervals_required){
    stop(
      "Confidence intervals for ratios have not yet been implemented.",
      call. = FALSE
    )
  }
  
  # Calculate ratios
  result$value[eligible] <-
    result[[numerator_col]][eligible] /
    result[[denominator_col]][eligible] *
    result[[value_multiplier_col]][eligible]
  
  result
}