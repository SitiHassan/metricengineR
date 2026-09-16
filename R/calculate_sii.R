#' Calculate the Slope Index of Inequality
#'
#' Calculates the Slope Index of Inequality (SII) for a binary outcome measured
#' across five deprivation quintiles. A separate inverse-variance-weighted
#' linear regression model is fitted for each grouping level.
#'
#' Each grouping level must contain exactly one row for each deprivation
#' quintile from 1 to 5. The numerator and denominator are used to calculate
#' the outcome proportion within each quintile.
#'
#' @section Calculation:
#'
#' For each deprivation quintile, the outcome proportion is calculated as:
#'
#' \deqn{p = \frac{numerator}{denominator}}
#'
#' A weighted linear regression is then fitted:
#'
#' \deqn{p = intercept + slope \times deprivation\ position}
#'
#' The regression uses the five quintile-specific proportions rather than a
#' single pooled proportion.
#'
#' @section Deprivation scaling:
#'
#' Deprivation quintiles are assigned evenly spaced relative deprivation
#' positions:
#'
#' \itemize{
#'   \item Quintile 1, most deprived: 0.9
#'   \item Quintile 2: 0.7
#'   \item Quintile 3: 0.5
#'   \item Quintile 4: 0.3
#'   \item Quintile 5, least deprived: 0.1
#' }
#'
#' The full deprivation scale runs from 0 to 1. Under this scaling, the
#' regression slope represents the modelled difference in the outcome
#' proportion between the least and most deprived population extremes.
#'
#' @section Regression weighting:
#'
#' Each quintile is weighted using the inverse of its estimated binomial
#' variance:
#'
#' \deqn{variance = \frac{p(1-p)}{n}}
#'
#' \deqn{weight = \frac{1}{variance}}
#'
#' where \eqn{p} is the quintile-specific outcome proportion and \eqn{n} is
#' its denominator.
#'
#' Quintile estimates with greater statistical precision receive more weight
#' in the regression.
#'
#' @section Interpretation:
#'
#' The signed SII is the regression slope multiplied by 100 and is expressed
#' in percentage points:
#'
#' \deqn{signed\ SII = slope \times 100}
#'
#' Under the scaling used by this function:
#'
#' \itemize{
#'   \item a positive signed SII indicates that the modelled outcome is higher
#'   at the most deprived extreme;
#'   \item a negative signed SII indicates that the modelled outcome is lower
#'   at the most deprived extreme;
#'   \item a signed SII close to zero indicates little modelled socioeconomic
#'   gradient.
#' }
#'
#' The returned \code{value} is the absolute SII:
#'
#' \deqn{absolute\ SII = |slope \times 100|}
#'
#' It represents the size of the modelled deprivation gap in percentage
#' points, regardless of direction.
#'
#' The SII is not simply the observed difference between deprivation quintiles
#' 1 and 5. It uses a weighted regression fitted across all five quintiles.
#' 
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param quintile_col Name of the deprivation quintile column.
#'   Defaults to `"imd_code"`.
#' @param numerator_col Name of the numerator column.
#'   Defaults to `"numerator"`.
#' @param denominator_col Name of the denominator column.
#'   Defaults to `"denominator"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be calculated. Defaults to `FALSE`.
#'
#' @return A data frame containing one row per SII calculation with `value`,
#'   `lowercl`, and `uppercl` columns.
#'
#' @details
#' Each SII calculation must contain exactly one row for deprivation quintiles
#' 1 to 5. A weighted linear regression is fitted using quintile-specific
#' proportions and inverse-variance weights.
#'
#' The returned `value` is the absolute regression slope multiplied by 100 and
#' represents the deprivation gap in percentage points.
#'
#' Confidence intervals are not currently implemented.
#'
#' @export
calculate_sii <- function(
    df,
    value_type_code_col = "value_type_code",
    quintile_col = "imd_code",
    numerator_col = "numerator",
    denominator_col = "denominator",
    confidence_intervals_required = FALSE){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
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
  
  sii_keys <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "aggregation_id",
    "age_group_code",
    "sex_code",
    "ethnicity_code",
    "creation_date",
    value_type_code_col,
    "source_code",
    "time_period_type",
    "combination_id"
  )
  
  required_cols <- unique(
    c(
      sii_keys,
      quintile_col,
      numerator_col,
      denominator_col
    )
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0){
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if(confidence_intervals_required){
    stop(
      "Confidence intervals for SII have not yet been implemented.",
      call. = FALSE
    )
  }
  
  # Prepare SII rows
  df_calc <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 14L
    ) |>
    dplyr::mutate(
      quintile_sii = suppressWarnings(
        as.integer(as.character(.data[[quintile_col]]))
      ),
      numerator_sii = .data[[numerator_col]],
      denominator_sii = .data[[denominator_col]]
    )
  
  # Handle no SII rows
  if(nrow(df_calc) == 0){
    
    result <- df_calc |>
      dplyr::mutate(
        value = numeric(),
        lowercl = numeric(),
        uppercl = numeric()
      )
    
    return(result)
  }
  
  # Validate numerator and denominator
  if(!is.numeric(df_calc$numerator_sii) ||
     !is.numeric(df_calc$denominator_sii)){
    
    stop(
      "Numerator and denominator must be numeric.",
      call. = FALSE
    )
  }
  
  if(anyNA(df_calc$numerator_sii) ||
     anyNA(df_calc$denominator_sii)){
    
    stop(
      "Numerator and denominator cannot contain missing values.",
      call. = FALSE
    )
  }
  
  if(any(df_calc$denominator_sii <= 0)){
    stop(
      "Denominator must be greater than zero.",
      call. = FALSE
    )
  }
  
  if(any(df_calc$numerator_sii < 0)){
    stop(
      "Numerator cannot be negative.",
      call. = FALSE
    )
  }
  
  if(any(df_calc$numerator_sii > df_calc$denominator_sii)){
    stop(
      "Numerator cannot be greater than denominator.",
      call. = FALSE
    )
  }
  
  # Check each group contains quintiles 1 to 5
  quintile_check <- df_calc |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(sii_keys)
      )
    ) |>
    dplyr::summarise(
      valid_quintiles =
        dplyr::n() == 5 &&
        setequal(.data$quintile_sii, 1:5),
      .groups = "drop"
    )
  
  if(any(!quintile_check$valid_quintiles)){
    stop(
      "Each SII calculation must contain exactly one row for IMD quintiles 1 to 5.",
      call. = FALSE
    )
  }
  
  # Create variables used in the regression
  model_data <- df_calc |>
    dplyr::mutate(
      deprivation_position = dplyr::recode(
        .data$quintile_sii,
        `1` = 0.9,
        `2` = 0.7,
        `3` = 0.5,
        `4` = 0.3,
        `5` = 0.1
      ),
      proportion =
        .data$numerator_sii /
        .data$denominator_sii,
      variance =
        .data$proportion *
        (1 - .data$proportion) /
        .data$denominator_sii
    )
  
  if(any(
    model_data$variance <= 0 |
    !is.finite(model_data$variance)
  )){
    stop(
      "SII variance must be positive and finite.",
      call. = FALSE
    )
  }
  
  # Calculate inverse variance weights and nest each SII group
  model_data <- model_data |>
    dplyr::mutate(
      inverse_variance_weight =
        1 / .data$variance
    ) |>
    tidyr::nest(
      data = -dplyr::all_of(sii_keys)
    )
  
  # Fit one model per SII group
  result <- model_data |>
    dplyr::mutate(
      model = purrr::map(
        .data$data,
        ~ stats::lm(
          proportion ~ deprivation_position,
          data = .x,
          weights = .x$inverse_variance_weight
        )
      ),
      numerator = purrr::map_dbl(
        .data$data,
        ~ sum(.x$numerator_sii)
      ),
      denominator = purrr::map_dbl(
        .data$data,
        ~ sum(.x$denominator_sii)
      ),
      value = purrr::map_dbl(
        .data$model,
        ~ abs(
          unname(
            stats::coef(.x)[["deprivation_position"]]
          ) * 100
        )
      ),
      lowercl = NA_real_,
      uppercl = NA_real_
    ) |>
    dplyr::transmute(
      dplyr::across(
        dplyr::all_of(sii_keys)
      ),
      numerator = .data$numerator,
      denominator = .data$denominator,
      value = .data$value,
      lowercl = .data$lowercl,
      uppercl = .data$uppercl,
      imd_code = 999L
    ) |>
    dplyr::ungroup()
  
  result
}