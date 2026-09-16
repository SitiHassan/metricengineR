#' Calculate the Slope Index of Inequality
#'
#' Calculates the Slope Index of Inequality (SII) for a binary outcome measured
#' across ordered deprivation groups. Only rows with value type code 14 are
#' included in the calculation.
#'
#' A separate inverse-variance-weighted linear regression model is fitted for
#' each grouping level. Each grouping level must contain exactly one row for
#' each deprivation group.
#'
#' The numerator and denominator are used to calculate the outcome proportion
#' within each deprivation group.
#'
#' @section Calculation:
#'
#' For each deprivation group, the outcome proportion is calculated as:
#'
#' \deqn{p = \frac{numerator}{denominator}}
#'
#' A weighted linear regression is then fitted:
#'
#' \deqn{p = intercept + slope \times deprivation\ position}
#'
#' The regression uses the group-specific proportions rather than a single
#' pooled proportion.
#'
#' @section Deprivation scaling:
#'
#' Deprivation groups are assigned evenly spaced relative deprivation positions
#' across a scale from 0 to 1.
#'
#' The position for each group is calculated as:
#'
#' \deqn{position = 1 - \frac{group - 0.5}{N}}
#'
#' where \eqn{N} is the number of deprivation groups.
#'
#' For five deprivation quintiles, this gives:
#'
#' \itemize{
#'   \item Group 1, most deprived: 0.9
#'   \item Group 2: 0.7
#'   \item Group 3: 0.5
#'   \item Group 4: 0.3
#'   \item Group 5, least deprived: 0.1
#' }
#'
#' For ten deprivation deciles, the positions range from 0.95 for the most
#' deprived group to 0.05 for the least deprived group.
#'
#' Because the full deprivation scale runs from 0 to 1, the regression slope
#' represents the modelled difference in the outcome proportion between the
#' least and most deprived population extremes.
#'
#' @section Regression weighting:
#'
#' Each deprivation group is weighted using the inverse of its estimated
#' binomial variance:
#'
#' \deqn{variance = \frac{p(1-p)}{n}}
#'
#' \deqn{weight = \frac{1}{variance}}
#'
#' where \eqn{p} is the group-specific outcome proportion and \eqn{n} is its
#' denominator.
#'
#' Estimates with greater statistical precision receive more weight in the
#' regression.
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
#'   \item a negative signed SII indicates that the modelled outcome is higher
#'   at the least deprived extreme;
#'   \item a signed SII close to zero indicates little modelled socioeconomic
#'   gradient.
#' }
#'
#' The absolute SII is:
#'
#' \deqn{absolute\ SII = |slope \times 100|}
#'
#' It represents the size of the modelled deprivation gap in percentage points,
#' regardless of direction.
#'
#' The function returns both signed and absolute SII values, together with a
#' text description of the direction of the gradient. The `value_output`
#' argument determines which SII measure is placed in the standard `value`
#' column.
#'
#' The SII is not simply the observed difference between the most and least
#' deprived groups. It uses a weighted regression fitted across all deprivation
#' groups.
#'
#' @param df A data frame containing the values to calculate.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Rows where this column equals 14 are used for SII calculations.
#'   Defaults to `"value_type_code"`.
#' @param deprivation_group_col Name of the deprivation group column.
#'   Defaults to `"imd_code"`.
#' @param n_deprivation_groups Number of deprivation groups included in each
#'   SII calculation. Defaults to `5L` for quintiles. Use `10L` for deciles.
#' @param numerator_col Name of the numerator column.
#'   Defaults to `"numerator"`.
#' @param denominator_col Name of the denominator column.
#'   Defaults to `"denominator"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be calculated. Defaults to `FALSE`.
#' @param value_output Determines which SII measure is placed in the standard
#'   `value` column. Must be either `"absolute"` or `"signed"`.
#'   Defaults to `"absolute"`.
#'
#' @return A data frame containing one row per SII calculation with `value`,
#'   `lowercl`, `uppercl`, `sii_signed_percentage_points`,
#'   `sii_absolute_percentage_points`, and `sii_direction`.
#'
#' @details
#' Rows with value type codes other than 14 are excluded before calculation.
#'
#' Each SII calculation must contain exactly one row for deprivation groups
#' 1 to `n_deprivation_groups`. Numerators and denominators must represent valid
#' proportions, and each deprivation group must have a positive finite
#' variance.
#'
#' Confidence intervals are not currently implemented.
#'
#' @export
calculate_sii <- function(
    df,
    value_type_code_col = "value_type_code",
    deprivation_group_col = "imd_code",
    n_deprivation_groups = 5L,
    numerator_col = "numerator",
    denominator_col = "denominator",
    confidence_intervals_required = FALSE,
    value_output = "absolute"){
  
  # Validate input
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.numeric(n_deprivation_groups) ||
     length(n_deprivation_groups) != 1 ||
     is.na(n_deprivation_groups) ||
     !is.finite(n_deprivation_groups) ||
     n_deprivation_groups < 2 ||
     n_deprivation_groups %% 1 != 0){
    
    stop(
      "`n_deprivation_groups` must be a whole number greater than 1.",
      call. = FALSE
    )
  }
  
  n_deprivation_groups <- as.integer(
    n_deprivation_groups
  )
  
  if(!is.logical(confidence_intervals_required) ||
     length(confidence_intervals_required) != 1 ||
     is.na(confidence_intervals_required)){
    
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  if(!is.character(value_output) ||
     length(value_output) != 1 ||
     is.na(value_output) ||
     !value_output %in% c("absolute", "signed")){
    
    stop(
      "`value_output` must be either \"absolute\" or \"signed\".",
      call. = FALSE
    )
  }
  
  # Define SII grouping columns
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
  
  # Check required columns
  required_cols <- unique(
    c(
      sii_keys,
      deprivation_group_col,
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
  
  # Confidence intervals are not currently supported
  if(confidence_intervals_required){
    stop(
      "Confidence intervals for SII have not yet been implemented.",
      call. = FALSE
    )
  }
  
  # Filter and prepare SII rows
  df_calc <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 14L
    ) |>
    dplyr::mutate(
      deprivation_group_sii = suppressWarnings(
        as.integer(
          as.character(
            .data[[deprivation_group_col]]
          )
        )
      ),
      numerator_sii = .data[[numerator_col]],
      denominator_sii = .data[[denominator_col]]
    )
  
  # Handle no SII rows
  if(nrow(df_calc) == 0){
    
    result <- df_calc |>
      dplyr::select(
        dplyr::all_of(sii_keys)
      ) |>
      dplyr::mutate(
        numerator = numeric(),
        denominator = numeric(),
        value = numeric(),
        lowercl = numeric(),
        uppercl = numeric(),
        imd_code = integer(),
        sii_signed_percentage_points = numeric(),
        sii_absolute_percentage_points = numeric(),
        sii_direction = character()
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
  
  if(any(
    df_calc$numerator_sii >
    df_calc$denominator_sii
  )){
    stop(
      "Numerator cannot be greater than denominator.",
      call. = FALSE
    )
  }
  
  # Check deprivation groups
  deprivation_check <- df_calc |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(sii_keys)
      )
    ) |>
    dplyr::summarise(
      row_count = dplyr::n(),
      distinct_groups = dplyr::n_distinct(
        .data$deprivation_group_sii
      ),
      correct_group_set = setequal(
        .data$deprivation_group_sii,
        seq_len(n_deprivation_groups)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      valid_groups =
        .data$row_count == n_deprivation_groups &
        .data$distinct_groups == n_deprivation_groups &
        .data$correct_group_set
    )
  
  if(any(!deprivation_check$valid_groups)){
    stop(
      "Each SII calculation must contain exactly one row for deprivation groups 1 to ",
      n_deprivation_groups,
      ".",
      call. = FALSE
    )
  }
  
  # Calculate deprivation position, proportion and variance
  model_data <- df_calc |>
    dplyr::mutate(
      
      deprivation_position =
        1 - (
          (.data$deprivation_group_sii - 0.5) /
            n_deprivation_groups
        ),
      
      proportion =
        .data$numerator_sii /
        .data$denominator_sii,
      
      variance =
        .data$proportion *
        (1 - .data$proportion) /
        .data$denominator_sii
    )
  
  # Validate variance
  if(any(
    model_data$variance <= 0 |
    !is.finite(model_data$variance)
  )){
    stop(
      "SII variance must be positive and finite.",
      call. = FALSE
    )
  }
  
  # Calculate inverse variance weights
  model_data <- model_data |>
    dplyr::mutate(
      inverse_variance_weight =
        1 / .data$variance
    )
  
  # Create one nested dataset per SII calculation
  model_data <- model_data |>
    tidyr::nest(
      data = -dplyr::all_of(sii_keys)
    )
  
  # Fit weighted regression and calculate SII
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
      
      sii_signed_percentage_points = purrr::map_dbl(
        .data$model,
        ~ unname(
          stats::coef(.x)[["deprivation_position"]]
        ) * 100
      ),
      
      sii_absolute_percentage_points =
        abs(.data$sii_signed_percentage_points),
      
      sii_direction = dplyr::case_when(
        .data$sii_signed_percentage_points > 0 ~
          "Higher in most deprived",
        
        .data$sii_signed_percentage_points < 0 ~
          "Higher in least deprived",
        
        TRUE ~
          "No deprivation gradient"
      ),
      
      value = if(value_output == "absolute"){
        .data$sii_absolute_percentage_points
      } else {
        .data$sii_signed_percentage_points
      },
      
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
      
      # SII represents all deprivation groups
      imd_code = 999L,
      
      sii_signed_percentage_points =
        .data$sii_signed_percentage_points,
      
      sii_absolute_percentage_points =
        .data$sii_absolute_percentage_points,
      
      sii_direction =
        .data$sii_direction
    ) |>
    dplyr::ungroup()
  
  result
}