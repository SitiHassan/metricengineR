#' Calculate differences between two values
#'
#' Calculates the difference between a minuend and subtrahend for selected
#' value type codes.
#'
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param value_type_codes Value type codes to include in the calculation.
#'   Defaults to `c(10L, 11L)`.
#' @param minuend_col Name of the column containing the value to subtract from.
#'   Defaults to `"numerator"`.
#' @param subtrahend_col Name of the column containing the value to subtract.
#'   Defaults to `"denominator"`.
#'
#' @return A data frame containing the selected rows with:
#'   `value` calculated as minuend minus subtrahend, and `lowercl` and
#'   `uppercl` set to `NA`.
#'
#' @details
#' By default, value type code 10 represents percentage point difference and
#' value type code 11 represents crude difference.
#'
#' The function checks that the input is a data frame and that the required
#' columns are present before calculation.
#'
#' @export
calc_difference <- function(df, 
                            value_type_code_col = "value_type_code", 
                            value_type_codes = c(10L, 11L), # 10 = percentage point difference; 11 = crude difference
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
      .data[[value_type_code_col]] %in% value_type_codes
      ) |> 
    dplyr::mutate(
      value = .data[[minuend_col]] - .data[[subtrahend_col]],
      lowercl = NA_real_,
      uppercl = NA_real_
    )
  
  result
}

calc_percentage_change <- function(df, 
                                   value_type_code_col = "value_type_code", 
                                   value_type_codes = c(9),
                                   new_value_col = "numerator",
                                   old_value_col = "denominator",
                                   value_multiplier_col = "value_multiplier",
                                   confidence_intervals_required = FALSE){
  
  result <- df |> 
    filter(df[[value_type_code_col]] %in% value_type_codes) |> 
    mutate(
      value = if_else(
        df[[old_value_col]] == 0 | is.na(df[[old_value_col]]),
        NA_real_, # ignore 0/NA denominator as sometimes there are no plan/previous/baseline values
        (df[[new_value_col]] - df[[old_value_col]]) / df[[old_value_col]] * df[[value_multiplier_col]]
      ),
      lowercl = NA_real_,
      uppercl = NA_real_
    )
  
  return(result)
}

calc_percentage <- function(df,
                            value_type_code_col = "value_type_code",
                            value_type_code = c(2),
                            value_multiplier_col = "value_multiplier",
                            numerator_col = "numerator",
                            denominator_col = "denominator",
                            confidence_intervals_required = FALSE){
  
  # Inputs:
  #   df_in - dataframe eligible for processing, containing `numerator`, `denominator`,
  #           `value_multiplier`, and `value_type_code` (2 = percentage)
  # Output:
  #   The same rows (where value_type_code == 2) with percentage and 95% CI
  #   columns added by PHEindicatormethods::phe_proportion:
  #   `value`, `lowercl`, `uppercl`, `method`
  
  df_percentage <- df %>%
    filter(df[[value_type_code_col]] %in% value_type_code)
  
  df_proportion <- df_percentage |> 
    filter(!is.na(denominator),
           denominator != 0,
           numerator <= denominator)
  
  multiplier_value <- df_proportion[[value_multiplier]][1]
  
  df_proportion <- df_proportion |> 
    phe_proportion(
      x = df[[numerator_col]],
      n = df[[denominator_col]],
      confidence = 0.95,
      type = "standard",
      multiplier = multiplier_value
    )
  
  # Achievement against target, growth or increase, or repeat events
  df_other_percentage <- df_perc |> 
    filter(is.na(denominator) |
             denominator == 0 |
             numerator > denominator) |> 
    mutate(
      value = if_else(
        !is.na(denominator) & denominator != 0,
        (numerator / denominator) * value_multiplier,
        NA_real_
      ),
      lowercl = NA_real_,
      uppercl = NA_real_,
    )
  
  output <- bind_rows(
    df_proportion,
    df_other_percentage
  )
  
  return(output)
  
}