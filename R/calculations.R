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