#' Create Pooled Multi-Year Data
#'
#' Creates pooled multi-year data from yearly Calendar or Financial year records.
#' Numerators and denominators are summed across rolling year windows such as
#' 3-year or 5-year periods.
#'
#' @param df A data frame containing yearly records to be pooled. It must include
#'   the pooling key columns, `start_date`, `numerator`, `denominator`, and
#'   `period_type`.
#' @param span_years Integer vector giving the number of years to include in each
#'   pooled period. Defaults to `c(3L, 5L)`.
#' @param pool_keys Character vector of column names used to group records before
#'   pooling.
#'
#' @return A data frame containing pooled records with summed `numerator` and
#'   `denominator` values and newly created `start_date`, `end_date`, and
#'   `time_period_type` columns.
#'
#' @details
#' Only rows where `period_type` is `"Calendar"` or `"Financial"` are included.
#'
#' Calendar pooled periods run from 1 January to 31 December. Financial year
#' pooled periods run from 1 April to 31 March.
#'
#' Only complete pooling windows are returned. For example, a 3-year pooled
#' result is only created when all three years are present.
#'
#' @export
create_pooled_data <- function(
    df,
    span_years = c(3L, 5L),
    pool_keys = c(
      "indicator_id",
      "imd_code",
      "aggregation_id",
      "age_group_code",
      "sex_code",
      "ethnicity_code",
      "creation_date",
      "value_type_code",
      "source_code",
      "combination_id"
    )) {
  
  # Validate inputs 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(
    !is.numeric(span_years) ||
    length(span_years) == 0L ||
    any(is.na(span_years)) ||
    any(!is.finite(span_years)) ||
    any(span_years %% 1 != 0) ||
    any(span_years < 1)
  ){
    stop(
      "`span_years` must contain whole numbers greater than or equal to 1.",
      call. = FALSE
    )
  }
  
  span_years <- as.integer(span_years)
  
  required_cols <- c(
    pool_keys,
    "start_date", # end_date is not required for the calculation of pooled periods
    "numerator",
    "denominator",
    "period_type"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0){
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # Create yearly data 
  
  yearly <- df |>
    dplyr::mutate(
      period_year = lubridate::year(
        as.Date(.data$start_date)
      )
    ) |>
    dplyr::filter(
      .data$period_type %in% c(
        "Calendar",
        "Financial"
      ),
      !is.na(.data$period_year)
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(pool_keys)
      ),
      .data$period_type,
      .data$period_year
    ) |>
    dplyr::summarise(
      numerator = sum(
        .data$numerator,
        na.rm = TRUE
      ),
      denominator = sum(
        .data$denominator,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # Return empty result if there is no yearly data 
  
  if(nrow(yearly) == 0L){
    return(
      dplyr::slice(df, 0)
    )
  }
  
  
  # Determine available year range 
  
  min_year <- min(
    yearly$period_year,
    na.rm = TRUE
  )
  
  max_year <- max(
    yearly$period_year,
    na.rm = TRUE
  )
  
  
  # Generate pooling windows 
  
  ranges <- lapply(
    span_years,
    function(k){
      
      generate_year_windows(
        min_year = min_year,
        max_year = max_year,
        span_years = k
      )
    }
  ) |>
    dplyr::bind_rows()
  
  
  if(nrow(ranges) == 0L){
    return(
      dplyr::slice(df, 0)
    )
  }
  
  
  # Match yearly records to pooling windows 
  
  pooled <- yearly |>
    dplyr::inner_join(
      ranges,
      by = dplyr::join_by(
        period_year >= from,
        period_year <= to
      )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(pool_keys)
      ),
      .data$period_type,
      .data$from,
      .data$to,
      .data$k
    ) |>
    dplyr::summarise(
      numerator = sum(
        .data$numerator,
        na.rm = TRUE
      ),
      denominator = sum(
        .data$denominator,
        na.rm = TRUE
      ),
      years_present = dplyr::n_distinct(
        .data$period_year
      ),
      .groups = "drop"
    ) |>
    
    # Only keep complete pooling windows
    dplyr::filter(
      .data$years_present == .data$k
    ) |>
    
    dplyr::mutate(
      
      start_date = dplyr::if_else(
        .data$period_type == "Calendar",
        lubridate::make_date(
          .data$from,
          1,
          1
        ),
        lubridate::make_date(
          .data$from,
          4,
          1
        )
      ),
      
      end_date = dplyr::if_else(
        .data$period_type == "Calendar",
        lubridate::make_date(
          .data$to,
          12,
          31
        ),
        lubridate::make_date(
          .data$to + 1L,
          3,
          31
        )
      ),
      
      time_period_type = paste0(
        .data$k,
        " year pooled"
      ),
      
      indicator_value = NA_real_,
      lower_ci95 = NA_real_,
      upper_ci95 = NA_real_
    ) |>
    dplyr::select(
      -dplyr::all_of(
        c(
          "from",
          "to",
          "k",
          "years_present",
          "period_type"
        )
      )
    )
  
  pooled
}