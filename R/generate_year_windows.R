#' Generate Rolling Year Windows
#'
#' Generates all valid rolling year windows between a minimum and maximum year.
#'
#' The size of each window is controlled by `span_years`. For example,
#' `span_years = 3` creates rolling 3-year periods such as 2014-2016,
#' 2015-2017, and 2016-2018.
#'
#' @param min_year First year available for the calculation.
#' @param max_year Last year available for the calculation.
#' @param span_years Number of years included in each rolling window.
#'   Defaults to `3L`.
#'
#' @return A data frame containing:
#'
#' \itemize{
#'   \item `from` - first year in the rolling window
#'   \item `to` - last year in the rolling window
#'   \item `k` - number of years included in the window
#' }
#'
#' If the available range is shorter than `span_years`, an empty data frame
#' is returned.
#'
#' @details
#' The function creates overlapping rolling windows. For example, using
#' `min_year = 2014`, `max_year = 2024`, and `span_years = 3` produces
#' windows from 2014-2016 through to 2022-2024.
#'
#' @examples
#' generate_year_windows(
#'   min_year = 2014,
#'   max_year = 2024,
#'   span_years = 3
#' )
#'
#' generate_year_windows(
#'   min_year = 2014,
#'   max_year = 2024,
#'   span_years = 5
#' )
#'
#' @export
generate_year_windows <- function(
    min_year,
    max_year,
    span_years = 3L
) {
  
  # Validate inputs
  if(
    !is.numeric(min_year) ||
    length(min_year) != 1L ||
    is.na(min_year) ||
    !is.finite(min_year) ||
    min_year %% 1 != 0
  ){
    stop(
      "`min_year` must be a single whole number.",
      call. = FALSE
    )
  }
  
  if(
    !is.numeric(max_year) ||
    length(max_year) != 1L ||
    is.na(max_year) ||
    !is.finite(max_year) ||
    max_year %% 1 != 0
  ){
    stop(
      "`max_year` must be a single whole number.",
      call. = FALSE
    )
  }
  
  if(
    !is.numeric(span_years) ||
    length(span_years) != 1L ||
    is.na(span_years) ||
    !is.finite(span_years) ||
    span_years %% 1 != 0 ||
    span_years < 1
  ){
    stop(
      "`span_years` must be a whole number greater than or equal to 1.",
      call. = FALSE
    )
  }
  
  min_year <- as.integer(min_year)
  max_year <- as.integer(max_year)
  span_years <- as.integer(span_years)
  
  if(min_year > max_year){
    stop(
      "`min_year` cannot be greater than `max_year`.",
      call. = FALSE
    )
  }
  
  gap <- span_years - 1L
  
  if((max_year - min_year) < gap){
    return(
      data.frame(
        from = integer(),
        to = integer(),
        k = integer()
      )
    )
  }
  
  starts <- seq.int(
    min_year,
    max_year - gap
  )
  
  data.frame(
    from = starts,
    to = starts + gap,
    k = rep(span_years, length(starts))
  )
}