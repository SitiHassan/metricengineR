library(dplyr)

# Function to create pooled data -----------------------------------------------
# Inputs:
#   df  - data frame containing at least POOL_KEYS, `start_date`, `numerator`,
#         `denominator`, `period_type` ("Calendar"/"Financial"), and `population_type`
#   ks  - integer vector of window sizes to pool over (e.g., c(3, 5))
#   time_period_3yrs (optional) - data frame with columns `from`, `to` defining
#         explicit 3-year windows; if NULL, windows are auto-generated from data
#   time_period_5yrs (optional) - data frame with columns `from`, `to` defining
#         explicit 5-year windows; if NULL, windows are auto-generated from data
#
# Output:
#   A data frame containing only the 3- and/or 5-year pooled rows (no yearly rows),
#   where `numerator` and `denominator` are summed over each window, dates are set
#   from `from`/`to` according to `period_type`, and `time_period_type` is
#   "3 year pooled" or "5 year pooled".
#
# Notes/Assumptions:
#   - Only rows with `population_type == "Census"` are considered.
#   - One yearly record per POOL_KEYS ? year is constructed internally before pooling.
#   - If custom window tables are provided, they must have integer `from`/`to` years.

POOL_KEYS <- c(
  "indicator_id","imd_code","aggregation_id",
  "age_group_code","sex_code","ethnicity_code",
  "creation_date","value_type_code","source_code","combination_id"
)


create_pooled_data_original <- function(df, ks = c(3,5),
                                      POOL_KEYS,
                                      time_period_3yrs = NULL,
                                      time_period_5yrs = NULL) {
  
  # Select only indicators which use Census as denominator source
  df <- df |> filter(population_type == "Census")
  
  # Group data by POOL KEYS and year
  yearly <- df |>
    mutate(period_year = lubridate::year(as.Date(start_date))) |>
    filter(period_type %in% c("Calendar","Financial"),
           !is.na(period_year)) |>
    group_by(across(all_of(POOL_KEYS)), period_type, period_year, ) |>
    summarise(
      numerator   = sum(numerator,   na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(yearly) == 0) return(slice(df, 0)) # empty in, empty out
  
  # Build range windows (use data span if not provided)
  miny <- min(yearly$period_year, na.rm = TRUE)
  maxy <- max(yearly$period_year, na.rm = TRUE)
  
  ranges_list <- list()
  if (3 %in% ks) {
    rng3 <- if (is.null(time_period_3yrs)) {
      # auto-generate from data
      tibble::tibble(from = seq.int(miny, maxy - 2), to = from + 2, k = 3L)
    } else {
      time_period_3yrs |>  transmute(from = as.integer(from), to = as.integer(to), k = 3L)
    }
    ranges_list <- c(ranges_list, list(rng3))
  }
  if (5 %in% ks) {
    rng5 <- if (is.null(time_period_5yrs)) {
      tibble(from = seq.int(miny, maxy - 4), to = from + 4, k = 5L)
    } else {
      time_period_5yrs |>  transmute(from = as.integer(from), to = as.integer(to), k = 5L)
    }
    ranges_list <- c(ranges_list, list(rng5))
  }
  ranges <- bind_rows(ranges_list)
  
  # If the ranges sit outside the data years, drop those that cant match
  ranges <- ranges |>  filter(from >= miny, to <= maxy)
  
  if (nrow(ranges) == 0) {
    return(df |>  slice(0))
  }
  
  # Non-equi join each yearly row to all windows that cover its year
  pooled <- yearly |>
    inner_join(ranges, by = join_by(period_year >= from, period_year <= to)) |>
    group_by(across(all_of(POOL_KEYS)), period_type, from, to, k) |>
    summarise(
      numerator   = sum(numerator,   na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Map window start/end to actual dates by period_type
      start_date = if_else(
        period_type == "Calendar",
        lubridate::make_date(from, 1, 1),
        lubridate::make_date(from, 4, 1)
      ),
      end_date = if_else(
        period_type == "Calendar",
        lubridate::make_date(to, 12, 31),
        lubridate::make_date(to + 1, 3, 31)
      ),
      time_period_type = paste0(k, " year pooled"),
      indicator_value = NA_real_, lower_ci95 = NA_real_, upper_ci95 = NA_real_
    ) |>
    transmute(
      indicator_id, start_date, end_date,
      numerator, denominator,
      indicator_value, lower_ci95, upper_ci95,
      imd_code, aggregation_id, age_group_code, sex_code, ethnicity_code,
      creation_date, value_type_code, source_code, time_period_type, combination_id
    )
  
  pooled
}

