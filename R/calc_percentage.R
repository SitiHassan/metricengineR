#' Calculate percentages
#'
#' Calculates percentage values for rows with value type code 2.
#' Confidence intervals can optionally be calculated using a proportion method.
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
#' @param ci_method Confidence interval method. Currently only `"proportion"`
#'   is supported. Defaults to `"proportion"`.
#'
#' @return A data frame containing rows with value type code 2 and the calculated
#'   columns `value`, `lowercl`, and `uppercl`.
#'
#' @details
#' Rows are eligible for calculation when numerator, denominator, and value
#' multiplier are present and the denominator is greater than zero.
#'
#' When confidence intervals are not required, percentage values are calculated
#' as numerator divided by denominator multiplied by the value multiplier.
#' Confidence interval columns are returned as `NA`.
#'
#' When confidence intervals are required, eligible rows must represent valid
#' proportions where the numerator is between zero and the denominator.
#' `PHEindicatormethods::phe_proportion()` is used to calculate the value and
#' 95 percent confidence intervals.
#'
#' Ineligible rows are retained and their calculated values remain `NA`.
#'
#' @export
calc_percentage <- function(df,
                            value_type_code_col = "value_type_code",
                            numerator_col = "numerator",
                            denominator_col = "denominator",
                            value_multiplier_col = "value_multiplier",
                            confidence_intervals_required = FALSE,
                            ci_method = "proportion"){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  # checks missing columns
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
  
  # Checks that confidence_intervals_required is a single TRUE or FALSE
  if(!is.logical(confidence_intervals_required) ||
     length(confidence_intervals_required) != 1 ||
     is.na(confidence_intervals_required)){
    
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # Filter percentage rows
  result <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 2L
    )
  
  # Define rows eligible for calculation
  # Each row much have a numerator, a denominator, the denominator being > 0, the multiplier exists
  eligible <- !is.na(result[[numerator_col]]) &
    !is.na(result[[denominator_col]]) &
    result[[denominator_col]] > 0 &
    !is.na(result[[value_multiplier_col]])
  
  # Initialise calculated columns
  result$value <- NA_real_
  result$lowercl <- NA_real_
  result$uppercl <- NA_real_
  
  # Calculate percentages without confidence intervals
  # Only for eligible rows
  if(!confidence_intervals_required){
    
    result$value[eligible] <-
      result[[numerator_col]][eligible] /
      result[[denominator_col]][eligible] *
      result[[value_multiplier_col]][eligible]
    
    return(result)
  }
  
  # If confidence intervals are required:
  # Validate confidence interval method
  if(!identical(ci_method, "proportion")){
    stop(
      "`ci_method` must be \"proportion\" for `calc_percentage()`.",
      call. = FALSE
    )
  }
  
  # Validate genuine proportions
  if(any(
    result[[numerator_col]][eligible] < 0 |
    result[[numerator_col]][eligible] >
    result[[denominator_col]][eligible]
  )){
    stop(
      "Invalid proportion: numerator must be between 0 and denominator.",
      call. = FALSE
    )
  }
  
  # Calculate proportion confidence intervals
  if(any(eligible)){
    
    df_proportion <- result[eligible, ] |>
      dplyr::mutate(
        numerator_ci = .data[[numerator_col]],
        denominator_ci = .data[[denominator_col]]
      )
    
    multiplier_values <- unique(
      df_proportion[[value_multiplier_col]]
    )
    
    if(length(multiplier_values) > 1){
      stop(
        "More than one value multiplier found for percentage calculations.",
        call. = FALSE
      )
    }
    
    multiplier_value <- multiplier_values[1]
    
    ci_result <- PHEindicatormethods::phe_proportion(
      df_proportion,
      x = numerator_ci,
      n = denominator_ci,
      confidence = 0.95,
      type = "standard",
      multiplier = multiplier_value
    )
    
    result$value[eligible] <- ci_result$value
    result$lowercl[eligible] <- ci_result$lowercl
    result$uppercl[eligible] <- ci_result$uppercl
  }
  
  result
}