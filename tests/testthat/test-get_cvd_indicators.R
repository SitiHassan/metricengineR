# Test 1: Invalid delay is rejected

testthat::test_that(
  "get_cvd_indicators validates delay_seconds",
  {
    
    testthat::expect_error(
      get_cvd_indicators(
        indicator_id = 1L,
        delay_seconds = -1
      ),
      "`delay_seconds` must be a single number greater than or equal to 0"
    )
    
    testthat::expect_error(
      get_cvd_indicators(
        indicator_id = 1L,
        delay_seconds = c(0.5, 1)
      ),
      "`delay_seconds` must be a single number greater than or equal to 0"
    )
    
    testthat::expect_error(
      get_cvd_indicators(
        indicator_id = 1L,
        delay_seconds = NA
      ),
      "`delay_seconds` must be a single number greater than or equal to 0"
    )
  }
)


# Test 2: combinations must be a data frame

testthat::test_that(
  "get_cvd_indicators validates combinations",
  {
    
    testthat::expect_error(
      get_cvd_indicators(
        combinations = c(1, 2),
        indicator_id = 1L
      ),
      "`combinations` must be a data frame"
    )
  }
)


# Test 3: combinations must contain required columns

testthat::test_that(
  "get_cvd_indicators requires combination columns",
  {
    
    combinations <- data.frame(
      time_period_id = 31L
    )
    
    testthat::expect_error(
      get_cvd_indicators(
        combinations = combinations,
        indicator_id = 1L
      ),
      "Columns not found in `combinations`: system_level_id"
    )
  }
)


# Test 4: Time period and system level must be supplied together

testthat::test_that(
  "get_cvd_indicators requires both time period and system level",
  {
    
    testthat::expect_error(
      get_cvd_indicators(
        time_period_id = 31L,
        indicator_id = 1L
      ),
      "Provide both `time_period_id` and `system_level_id`"
    )
    
    testthat::expect_error(
      get_cvd_indicators(
        system_level_id = 1L,
        indicator_id = 1L
      ),
      "Provide both `time_period_id` and `system_level_id`"
    )
  }
)


# Test 5: Supplied combinations are used

testthat::test_that(
  "get_cvd_indicators uses supplied combinations",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        data.frame(
          Value = 10
        )
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = c(31L, 33L),
      system_level_id = c(1L, 6L)
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = 7L,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      2L
    )
    
    testthat::expect_equal(
      result$data$time_period_id,
      c(31L, 33L)
    )
    
    testthat::expect_equal(
      result$data$system_level_id,
      c(1L, 6L)
    )
    
    testthat::expect_true(
      all(result$data$indicator_id == 7L)
    )
  }
)


# Test 6: Duplicate combinations are removed

testthat::test_that(
  "get_cvd_indicators removes duplicate combinations",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        data.frame(
          Value = 10
        )
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = c(31L, 31L),
      system_level_id = c(1L, 1L)
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = 7L,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      1L
    )
  }
)


# Test 7: Time period and system level vectors create all combinations

testthat::test_that(
  "get_cvd_indicators expands supplied time periods and system levels",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        data.frame(
          Value = 10
        )
      },
    .package = "cvdprevent"
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        time_period_id = c(31L, 33L),
        system_level_id = c(1L, 6L),
        indicator_id = 7L,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      4L
    )
  }
)


# Test 8: All indicators are retrieved when indicator_id is NULL

testthat::test_that(
  "get_cvd_indicators retrieves all available indicators when indicator_id is NULL",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      
      cvd_indicator_list = function(
    time_period_id,
    system_level_id
      ){
        
        data.frame(
          IndicatorID = c(1L, 7L)
        )
      },
    
    cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
    ){
      
      data.frame(
        Value = indicator_id * 10
      )
    },
    
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = 31L,
      system_level_id = 1L
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      sort(unique(result$data$indicator_id)),
      c(1L, 7L)
    )
    
    testthat::expect_equal(
      nrow(result$data),
      2L
    )
  }
)


# Test 9: Available combinations are retrieved automatically

testthat::test_that(
  "get_cvd_indicators retrieves available combinations when none are supplied",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      
      cvd_time_period_system_levels = function(){
        
        data.frame(
          TimePeriodID = c(31L, 33L),
          SystemLevelID = c(1L, 6L)
        )
      },
      
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        data.frame(
          Value = 10
        )
      },
    
    .package = "cvdprevent"
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        indicator_id = 7L,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      2L
    )
    
    testthat::expect_equal(
      result$data$time_period_id,
      c(31L, 33L)
    )
    
    testthat::expect_equal(
      result$data$system_level_id,
      c(1L, 6L)
    )
  }
)


# Test 10: Failed indicator requests are recorded

testthat::test_that(
  "get_cvd_indicators records failed indicator requests",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        if(indicator_id == 7L){
          stop("Test extraction error")
        }
        
        data.frame(
          Value = 10
        )
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = 31L,
      system_level_id = 1L
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = c(1L, 7L),
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      1L
    )
    
    testthat::expect_equal(
      result$data$indicator_id,
      1L
    )
    
    testthat::expect_equal(
      nrow(result$invalid_combinations),
      1L
    )
    
    testthat::expect_equal(
      result$invalid_combinations$indicator_id,
      7L
    )
    
    testthat::expect_match(
      result$invalid_combinations$error_message,
      "Test extraction error"
    )
  }
)


# Test 11: Extraction continues after one indicator fails

testthat::test_that(
  "get_cvd_indicators continues after an indicator request fails",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        if(indicator_id == 7L){
          stop("Test extraction error")
        }
        
        data.frame(
          Value = indicator_id
        )
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = 31L,
      system_level_id = 1L
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = c(1L, 7L, 50L),
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      result$data$indicator_id,
      c(1L, 50L)
    )
    
    testthat::expect_equal(
      result$invalid_combinations$indicator_id,
      7L
    )
  }
)


# Test 12: Indicator list failures are recorded

testthat::test_that(
  "get_cvd_indicators records failures when indicator lists cannot be retrieved",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_list = function(
    time_period_id,
    system_level_id
      ){
        
        stop("Indicator list unavailable")
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = 31L,
      system_level_id = 1L
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        delay_seconds = 0
      )
    )
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      nrow(result$invalid_combinations),
      1L
    )
    
    testthat::expect_true(
      is.na(result$invalid_combinations$indicator_id)
    )
    
    testthat::expect_match(
      result$invalid_combinations$error_message,
      "Indicator list unavailable"
    )
  }
)


# Test 13: Returned data uses cleaned column names

testthat::test_that(
  "get_cvd_indicators cleans returned column names",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::local_mocked_bindings(
      cvd_indicator_raw_data = function(
    time_period_id,
    system_level_id,
    indicator_id
      ){
        
        data.frame(
          "Indicator Value" = 25,
          check.names = FALSE
        )
      },
    .package = "cvdprevent"
    )
    
    combinations <- data.frame(
      time_period_id = 31L,
      system_level_id = 1L
    )
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = 1L,
        delay_seconds = 0
      )
    )
    
    testthat::expect_true(
      "indicator_value" %in% names(result$data)
    )
  }
)

# Test 14: Live CVDPREVENT extraction works

testthat::test_that(
  "get_cvd_indicators can retrieve live CVDPREVENT data",
  {
    
    testthat::skip_if_not_installed("cvdprevent")
    
    testthat::skip_if(
      Sys.getenv("RUN_CVD_LIVE_TESTS") != "true",
      "Live CVDPREVENT tests are not enabled"
    )
    
    combinations <- cvdprevent::cvd_time_period_system_levels() |>
      janitor::clean_names() |>
      dplyr::select(
        time_period_id,
        system_level_id
      ) |>
      dplyr::distinct() |>
      dplyr::slice(1)
    
    available_indicators <- cvdprevent::cvd_indicator_list(
      time_period_id = combinations$time_period_id[1],
      system_level_id = combinations$system_level_id[1]
    )
    
    indicator_id <- available_indicators$IndicatorID[1]
    
    suppressMessages(
      result <- get_cvd_indicators(
        combinations = combinations,
        indicator_id = indicator_id,
        delay_seconds = 0
      )
    )
    
    testthat::expect_true(
      is.data.frame(result$data)
    )
    
    testthat::expect_gt(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      nrow(result$invalid_combinations),
      0L
    )
  }
)