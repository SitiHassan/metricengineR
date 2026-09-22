# Helper data 

test_pool_keys <- c(
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
)


create_test_pool_data <- function(
    years = 2018:2022,
    indicator_id = 101L,
    period_type = "Calendar",
    numerators = seq(10, by = 10, length.out = length(years)),
    denominators = rep(100, length(years))
) {
  
  if(period_type == "Calendar"){
    start_dates <- as.Date(
      paste0(years, "-01-01")
    )
  } else {
    start_dates <- as.Date(
      paste0(years, "-04-01")
    )
  }
  
  data.frame(
    indicator_id = rep(indicator_id, length(years)),
    start_date = start_dates,
    numerator = numerators,
    denominator = denominators,
    imd_code = rep(999L, length(years)),
    aggregation_id = rep(151L, length(years)),
    age_group_code = rep(18L, length(years)),
    sex_code = rep(1L, length(years)),
    ethnicity_code = rep(999L, length(years)),
    creation_date = rep(
      as.Date("2026-01-01"),
      length(years)
    ),
    value_type_code = rep(2L, length(years)),
    source_code = rep(1L, length(years)),
    combination_id = rep(4L, length(years)),
    period_type = rep(period_type, length(years))
  )
}


# Test 1: Correct 3-year pooled data 

testthat::test_that(
  "create_pooled_data creates expected 3-year pooled data",
  {
    
    df <- create_test_pool_data()
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_equal(
      result$numerator,
      c(60, 90, 120)
    )
    
    testthat::expect_equal(
      result$denominator,
      c(300, 300, 300)
    )
    
    testthat::expect_equal(
      result$start_date,
      as.Date(
        c(
          "2018-01-01",
          "2019-01-01",
          "2020-01-01"
        )
      )
    )
    
    testthat::expect_equal(
      result$end_date,
      as.Date(
        c(
          "2020-12-31",
          "2021-12-31",
          "2022-12-31"
        )
      )
    )
    
    testthat::expect_equal(
      result$time_period_type,
      rep("3 year pooled", 3)
    )
  }
)


# Test 2: Correct 5-year pooled data 

testthat::test_that(
  "create_pooled_data creates expected 5-year pooled data",
  {
    
    df <- create_test_pool_data()
    
    result <- create_pooled_data(
      df,
      span_years = 5L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$numerator,
      150
    )
    
    testthat::expect_equal(
      result$denominator,
      500
    )
    
    testthat::expect_equal(
      result$start_date,
      as.Date("2018-01-01")
    )
    
    testthat::expect_equal(
      result$end_date,
      as.Date("2022-12-31")
    )
    
    testthat::expect_equal(
      result$time_period_type,
      "5 year pooled"
    )
  }
)


# Test 3: Multiple pooling periods 

testthat::test_that(
  "create_pooled_data supports multiple span lengths",
  {
    
    df <- create_test_pool_data()
    
    result <- create_pooled_data(
      df,
      span_years = c(3L, 5L),
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_equal(
      sum(result$time_period_type == "3 year pooled"),
      3L
    )
    
    testthat::expect_equal(
      sum(result$time_period_type == "5 year pooled"),
      1L
    )
  }
)


# Test 4: Financial year dates 

testthat::test_that(
  "create_pooled_data creates correct Financial year dates",
  {
    
    df <- create_test_pool_data(
      years = 2018:2020,
      period_type = "Financial"
    )
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      result$start_date,
      as.Date("2018-04-01")
    )
    
    testthat::expect_equal(
      result$end_date,
      as.Date("2021-03-31")
    )
    
    testthat::expect_equal(
      result$numerator,
      60
    )
    
    testthat::expect_equal(
      result$denominator,
      300
    )
  }
)


# Test 5: Incomplete pooling windows are removed 

testthat::test_that(
  "create_pooled_data only returns complete pooling windows",
  {
    
    df <- create_test_pool_data()
    
    # Remove 2019
    df <- df[
      df$start_date != as.Date("2019-01-01"),
    ]
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    # Only 2020-2022 contains all three years
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$start_date,
      as.Date("2020-01-01")
    )
    
    testthat::expect_equal(
      result$end_date,
      as.Date("2022-12-31")
    )
    
    testthat::expect_equal(
      result$numerator,
      120
    )
  }
)


# Test 6: Different groups are pooled separately 

testthat::test_that(
  "create_pooled_data pools different groups separately",
  {
    
    group_1 <- create_test_pool_data(
      indicator_id = 101L,
      numerators = c(10, 20, 30, 40, 50)
    )
    
    group_2 <- create_test_pool_data(
      indicator_id = 102L,
      numerators = c(5, 10, 15, 20, 25)
    )
    
    df <- dplyr::bind_rows(
      group_1,
      group_2
    )
    
    result <- create_pooled_data(
      df,
      span_years = 5L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      nrow(result),
      2L
    )
    
    testthat::expect_equal(
      result$numerator[
        result$indicator_id == 101L
      ],
      150
    )
    
    testthat::expect_equal(
      result$numerator[
        result$indicator_id == 102L
      ],
      75
    )
  }
)


# Test 7: Multiple records within a year are summed 

testthat::test_that(
  "create_pooled_data aggregates duplicate yearly records before pooling",
  {
    
    df <- create_test_pool_data(
      years = 2018:2020
    )
    
    duplicate_row <- df[1, ]
    
    duplicate_row$numerator <- 5
    duplicate_row$denominator <- 50
    
    df <- dplyr::bind_rows(
      df,
      duplicate_row
    )
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      result$numerator,
      65
    )
    
    testthat::expect_equal(
      result$denominator,
      350
    )
  }
)


# Test 8: Unsupported period types are excluded 

testthat::test_that(
  "create_pooled_data excludes unsupported period types",
  {
    
    df <- create_test_pool_data()
    
    df$period_type <- "Quarterly"
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 9: Output calculation columns are created 

testthat::test_that(
  "create_pooled_data creates expected output columns",
  {
    
    df <- create_test_pool_data()
    
    result <- create_pooled_data(
      df,
      span_years = 3L,
      pool_keys = test_pool_keys
    )
    
    testthat::expect_true(
      all(
        c(
          "start_date",
          "end_date",
          "numerator",
          "denominator",
          "indicator_value",
          "lower_ci95",
          "upper_ci95",
          "time_period_type"
        ) %in% names(result)
      )
    )
    
    testthat::expect_true(
      all(is.na(result$indicator_value))
    )
    
    testthat::expect_true(
      all(is.na(result$lower_ci95))
    )
    
    testthat::expect_true(
      all(is.na(result$upper_ci95))
    )
  }
)


# Test 10: Non-data-frame input is rejected 

testthat::test_that(
  "create_pooled_data rejects non-data-frame input",
  {
    
    testthat::expect_error(
      create_pooled_data(
        c(1, 2, 3),
        pool_keys = test_pool_keys
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 11: Invalid span_years are rejected 

testthat::test_that(
  "create_pooled_data validates span_years",
  {
    
    df <- create_test_pool_data()
    
    testthat::expect_error(
      create_pooled_data(
        df,
        span_years = 0,
        pool_keys = test_pool_keys
      ),
      "`span_years` must contain whole numbers greater than or equal to 1"
    )
    
    testthat::expect_error(
      create_pooled_data(
        df,
        span_years = 3.5,
        pool_keys = test_pool_keys
      ),
      "`span_years` must contain whole numbers greater than or equal to 1"
    )
    
    testthat::expect_error(
      create_pooled_data(
        df,
        span_years = NA,
        pool_keys = test_pool_keys
      ),
      "`span_years` must contain whole numbers greater than or equal to 1"
    )
    
    testthat::expect_error(
      create_pooled_data(
        df,
        span_years = "3",
        pool_keys = test_pool_keys
      ),
      "`span_years` must contain whole numbers greater than or equal to 1"
    )
  }
)


# Test 12: Missing required columns are reported 

testthat::test_that(
  "create_pooled_data reports missing required columns",
  {
    
    df <- create_test_pool_data() |>
      dplyr::select(
        -denominator
      )
    
    testthat::expect_error(
      create_pooled_data(
        df,
        pool_keys = test_pool_keys
      ),
      "Missing required columns: denominator"
    )
  }
)