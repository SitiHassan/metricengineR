#' Derive Time Period Type
#'
#' Classifies a date range into a standard metric-engine time period type based
#' on the difference between `start_date` and `end_date`.
#'
#' Supported time period types are:
#'
#' \itemize{
#'   \item `"Monthly"`
#'   \item `"Quarterly"`
#'   \item `"1 year"`
#'   \item `"3 year pooled"`
#'   \item `"5 year pooled"`
#' }
#'
#' @param start_date A Date vector containing the start date of each period.
#' @param end_date A Date vector containing the end date of each period.
#'
#' @return A character vector containing the derived time period type.
#'   Periods that do not match one of the supported types return `NA`.
#'
#' @details
#' The function accepts periods ending either on the anniversary date or one
#' day before the anniversary date.
#'
#' @examples
#' derive_time_period_type(
#'   start_date = as.Date("2024-01-01"),
#'   end_date = as.Date("2024-12-31")
#' )
#'
#' derive_time_period_type(
#'   start_date = as.Date("2024-01-01"),
#'   end_date = as.Date("2024-03-31")
#' )
#'
#' @export
derive_time_period_type <- function(
    start_date,
    end_date
) {
  
  # Validate inputs 
  
  if(!inherits(start_date, "Date")){
    stop(
      "`start_date` must be a Date vector.",
      call. = FALSE
    )
  }
  
  if(!inherits(end_date, "Date")){
    stop(
      "`end_date` must be a Date vector.",
      call. = FALSE
    )
  }
  
  if(length(start_date) != length(end_date)){
    stop(
      "`start_date` and `end_date` must have the same length.",
      call. = FALSE
    )
  }
  
  
  # Check whether dates match a specified period 
  
  matches_period <- function(period){
    
    anniversary <- lubridate::add_with_rollback(
      start_date,
      period
    )
    
    (end_date == anniversary) |
      (end_date == anniversary - lubridate::days(1))
  }
  
  
  # Derive time period type 
  
  dplyr::case_when(
    
    matches_period(
      lubridate::period(months = 1)
    ) ~ "Monthly",
    
    matches_period(
      lubridate::period(months = 3)
    ) ~ "Quarterly",
    
    matches_period(
      lubridate::period(years = 1)
    ) ~ "1 year",
    
    matches_period(
      lubridate::period(years = 3)
    ) ~ "3 year pooled",
    
    matches_period(
      lubridate::period(years = 5)
    ) ~ "5 year pooled",
    
    TRUE ~ NA_character_
  )
}

