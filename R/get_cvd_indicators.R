#' Get CVDPREVENT Indicators
#'
#' Retrieves indicator data from CVDPREVENT for selected or available
#' time period and system level combinations.
#'
#' @param time_period_id Optional vector of CVDPREVENT time period IDs.
#'   Must be supplied together with `system_level_id`.
#' @param system_level_id Optional vector of CVDPREVENT system level IDs.
#'   Must be supplied together with `time_period_id`.
#' @param indicator_id Optional vector of indicator IDs to retrieve.
#'   If `NULL`, all available indicators are retrieved for each combination.
#' @param combinations Optional data frame containing `time_period_id` and
#'   `system_level_id`. If supplied, these combinations are used directly.
#' @param delay_seconds Number of seconds to wait between indicator requests.
#'   Defaults to `0.5`.
#'
#' @return A list containing:
#' \itemize{
#'   \item `data` - combined data for successfully retrieved indicators.
#'   \item `invalid_combinations` - details of requests that could not be
#'   retrieved.
#' }
#'
#' @details
#' If no combinations, time periods, or system levels are supplied, all
#' available time period and system level combinations are retrieved
#' automatically from CVDPREVENT.
#'
#' If `indicator_id` is `NULL`, all available indicators are retrieved for
#' each combination.
#'
#' Failed requests are recorded and the function continues with the remaining
#' indicators.
#'
#' @export
get_cvd_indicators <- function(
    time_period_id = NULL,
    system_level_id = NULL,
    indicator_id = NULL,
    combinations = NULL,
    delay_seconds = 0.5
) {
  
  message("Starting CVDPREVENT extraction...")
  
  
  # Validate delay
  
  if(
    !is.numeric(delay_seconds) ||
    length(delay_seconds) != 1L ||
    is.na(delay_seconds) ||
    delay_seconds < 0
  ){
    stop(
      "`delay_seconds` must be a single number greater than or equal to 0.",
      call. = FALSE
    )
  }
  
  
  # Determine time period and system level combinations
  
  if(!is.null(combinations)){
    
    if(!is.data.frame(combinations)){
      stop(
        "`combinations` must be a data frame.",
        call. = FALSE
      )
    }
    
    required_cols <- c(
      "time_period_id",
      "system_level_id"
    )
    
    missing_cols <- setdiff(
      required_cols,
      names(combinations)
    )
    
    if(length(missing_cols) > 0L){
      stop(
        paste0(
          "Columns not found in `combinations`: ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    combinations <- combinations |>
      dplyr::select(
        dplyr::all_of(required_cols)
      ) |>
      dplyr::distinct()
    
    
  } else if(
    !is.null(time_period_id) &&
    !is.null(system_level_id)
  ){
    
    # Create combinations from supplied IDs
    
    combinations <- tidyr::expand_grid(
      time_period_id = time_period_id,
      system_level_id = system_level_id
    )
    
    
  } else if(
    is.null(time_period_id) &&
    is.null(system_level_id)
  ){
    
    # Retrieve all valid combinations from CVDPREVENT
    
    message(
      "No time periods or system levels supplied. ",
      "Retrieving all available combinations..."
    )
    
    combinations <- tryCatch(
      
      {
        
        cvdprevent::cvd_time_period_system_levels() |>
          janitor::clean_names() |>
          dplyr::select(
            dplyr::all_of(
              c("time_period_id",
                "system_level_id"
                )
            )
          ) |>
          dplyr::distinct()
        
      },
      
      error = function(e){
        
        stop(
          paste0(
            "Could not retrieve available CVDPREVENT combinations: ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
    
    
  } else {
    
    stop(
      "Provide both `time_period_id` and `system_level_id`, ",
      "or leave both as NULL.",
      call. = FALSE
    )
  }
  
  
  # Extract data for each combination
  
  results <- combinations |>
    purrr::pmap(
      function(time_period_id, system_level_id) {
        
        message(
          "Period: ",
          time_period_id,
          " | System level: ",
          system_level_id
        )
        
        tryCatch(
          {
            
            ids <- indicator_id
            
            
            # Retrieve available indicators if none are supplied
            
            if(is.null(ids)){
              
              indicators <- cvdprevent::cvd_indicator_list(
                time_period_id = time_period_id,
                system_level_id = system_level_id
              )
              
              ids <- unique(
                indicators$IndicatorID
              )
            }
            
            
            if(length(ids) == 0L){
              stop(
                "No indicators available.",
                call. = FALSE
              )
            }
            
            
            # Retrieve each indicator
            
            indicator_results <- purrr::map(
              seq_along(ids),
              function(i) {
                
                id <- ids[i]
                
                message(
                  "  Indicator: ",
                  id
                )
                
                result <- tryCatch(
                  
                  {
                    
                    data <- cvdprevent::cvd_indicator_raw_data(
                      time_period_id = time_period_id,
                      system_level_id = system_level_id,
                      indicator_id = id
                    ) |>
                      janitor::clean_names() |>
                      dplyr::mutate(
                        time_period_id = time_period_id,
                        system_level_id = system_level_id,
                        indicator_id = id
                      )
                    
                    list(
                      data = data,
                      error = NULL
                    )
                  },
                  
                  error = function(e){
                    
                    list(
                      data = NULL,
                      error = tibble::tibble(
                        time_period_id = time_period_id,
                        system_level_id = system_level_id,
                        indicator_id = id,
                        error_message = conditionMessage(e)
                      )
                    )
                  }
                )
                
                
                # Pause before next request
                
                if(i < length(ids)){
                  Sys.sleep(
                    delay_seconds
                  )
                }
                
                result
              }
            )
            
            
            list(
              data = indicator_results |>
                purrr::map("data") |>
                purrr::compact() |>
                dplyr::bind_rows(),
              
              error = indicator_results |>
                purrr::map("error") |>
                purrr::compact() |>
                dplyr::bind_rows()
            )
            
          },
          
          error = function(e){
            
            list(
              data = NULL,
              error = tibble::tibble(
                time_period_id = time_period_id,
                system_level_id = system_level_id,
                indicator_id = NA_integer_,
                error_message = conditionMessage(e)
              )
            )
          }
        )
      }
    )
  
  
  # Combine successful results
  
  raw_data <- results |>
    purrr::map("data") |>
    purrr::compact() |>
    dplyr::bind_rows()
  
  
  # Combine errors
  
  invalid_combinations <- results |>
    purrr::map("error") |>
    purrr::compact() |>
    dplyr::bind_rows()
  
  
  message(
    "\u2705 CVDPREVENT extraction completed."
  )
  
  
  # Return data and errors
  
  list(
    data = raw_data,
    invalid_combinations = invalid_combinations
  )
}