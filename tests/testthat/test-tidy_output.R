test_that("tidy_output returns the standard output columns in the correct order", {
  
  data <- data.frame(
    indicator_id = 1,
    start_date = as.Date("2026-01-01"),
    end_date = as.Date("2026-12-31"),
    numerator = 10,
    denominator = 100,
    indicator_value = 10,
    lower_ci95 = 8,
    upper_ci95 = 12,
    imd_code = 1,
    aggregation_id = 2,
    age_group_code = 3,
    sex_code = 1,
    ethnicity_code = 4,
    creation_date = as.POSIXct("2026-09-09 12:00:00"),
    value_type_code = 1,
    source_code = 2,
    time_period_type = "Annual",
    combination_id = 100
  )
  
  result <- tidy_output(data)
  
  expected_cols <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "numerator",
    "denominator",
    "indicator_value",
    "lower_ci95",
    "upper_ci95",
    "imd_code",
    "aggregation_id",
    "age_group_code",
    "sex_code",
    "ethnicity_code",
    "creation_date",
    "value_type_code",
    "source_code",
    "time_period_type",
    "combination_id"
  )
  
  expect_identical(
    names(result),
    expected_cols
  )
})

test_that("tidy_output maps value to indicator_value", {
  
  data <- data.frame(
    indicator_id = 1,
    value = 25
  )
  
  result <- tidy_output(data)
  
  expect_equal(
    result$indicator_value,
    25
  )
})

test_that("tidy_output maps lowercl and uppercl to confidence interval columns", {
  
  data <- data.frame(
    indicator_id = 1,
    value = 25,
    lowercl = 20,
    uppercl = 30
  )
  
  result <- tidy_output(data)
  
  expect_equal(
    result$lower_ci95,
    20
  )
  
  expect_equal(
    result$upper_ci95,
    30
  )
})


test_that("tidy_output preserves existing standard value columns", {
  
  data <- data.frame(
    indicator_id = 1,
    indicator_value = 50,
    value = 25,
    lower_ci95 = 40,
    lowercl = 20,
    upper_ci95 = 60,
    uppercl = 30
  )
  
  result <- tidy_output(data)
  
  expect_equal(
    result$indicator_value,
    50
  )
  
  expect_equal(
    result$lower_ci95,
    40
  )
  
  expect_equal(
    result$upper_ci95,
    60
  )
})

test_that("tidy_output adds missing standard columns", {
  
  data <- data.frame(
    indicator_id = 1,
    numerator = 10
  )
  
  result <- tidy_output(data)
  
  expected_cols <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "numerator",
    "denominator",
    "indicator_value",
    "lower_ci95",
    "upper_ci95",
    "imd_code",
    "aggregation_id",
    "age_group_code",
    "sex_code",
    "ethnicity_code",
    "creation_date",
    "value_type_code",
    "source_code",
    "time_period_type",
    "combination_id"
  )
  
  expect_true(
    all(expected_cols %in% names(result))
  )
  
  expect_true(
    is.na(result$denominator)
  )
  
  expect_true(
    is.na(result$indicator_value)
  )
})

test_that("tidy_output removes columns outside the standard schema", {
  
  data <- data.frame(
    indicator_id = 1,
    value = 25,
    temporary_column = "remove me",
    calculation_method = "test"
  )
  
  result <- tidy_output(data)
  
  expect_false(
    "temporary_column" %in% names(result)
  )
  
  expect_false(
    "calculation_method" %in% names(result)
  )
})

test_that("tidy_output preserves the number of rows", {
  
  data <- data.frame(
    indicator_id = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  
  result <- tidy_output(data)
  
  expect_equal(
    nrow(result),
    3
  )
  
  expect_equal(
    result$indicator_value,
    c(10, 20, 30)
  )
})

test_that("tidy_output rejects input that is not a data frame", {
  
  expect_error(
    tidy_output(c(1, 2, 3)),
    "`data` must be a data frame"
  )
})