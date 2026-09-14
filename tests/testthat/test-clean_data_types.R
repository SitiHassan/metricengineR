test_that("clean_data_types converts columns to expected types", {
  
  data <- data.frame(
    indicator_id = "1",
    start_date = "2026-01-01",
    end_date = "2026-12-31",
    numerator = "10",
    denominator = "100",
    indicator_value = "10.5",
    lower_ci95 = "9.5",
    upper_ci95 = "11.5",
    imd_code = "2",
    aggregation_id = "3",
    age_group_code = "4",
    sex_code = "1",
    ethnicity_code = "5",
    creation_date = "2026-09-09 12:00:00",
    value_type_code = "2",
    source_code = "7",
    time_period_type = 12,
    combination_id = "1001",
    stringsAsFactors = FALSE
  )
  
  result <- clean_data_types(data)
  
  expect_type(result$indicator_id, "integer")
  expect_s3_class(result$start_date, "Date")
  expect_s3_class(result$end_date, "Date")
  expect_type(result$numerator, "double")
  expect_type(result$denominator, "double")
  expect_type(result$indicator_value, "double")
  expect_type(result$lower_ci95, "double")
  expect_type(result$upper_ci95, "double")
  expect_type(result$imd_code, "integer")
  expect_type(result$aggregation_id, "integer")
  expect_type(result$age_group_code, "integer")
  expect_type(result$sex_code, "integer")
  expect_type(result$ethnicity_code, "integer")
  expect_s3_class(result$creation_date, "POSIXct")
  expect_type(result$value_type_code, "integer")
  expect_type(result$source_code, "integer")
  expect_type(result$time_period_type, "character")
  expect_type(result$combination_id, "integer")
})

test_that("clean_data_types preserves values during valid conversion", {
  
  data <- data.frame(
    indicator_id = "1",
    start_date = "2026-01-01",
    end_date = "2026-12-31",
    numerator = "10",
    denominator = "100",
    indicator_value = "10.5",
    lower_ci95 = "9.5",
    upper_ci95 = "11.5",
    imd_code = "2",
    aggregation_id = "3",
    age_group_code = "4",
    sex_code = "1",
    ethnicity_code = "5",
    creation_date = "2026-09-09 12:00:00",
    value_type_code = "2",
    source_code = "7",
    time_period_type = 12,
    combination_id = "1001",
    stringsAsFactors = FALSE
  )
  
  result <- clean_data_types(data)
  
  expect_equal(result$indicator_id, 1L)
  expect_equal(result$numerator, 10)
  expect_equal(result$denominator, 100)
  expect_equal(result$indicator_value, 10.5)
  expect_equal(result$imd_code, 2L)
  expect_equal(result$combination_id, 1001L)
  expect_equal(result$time_period_type, "12")
})

test_that("clean_data_types errors when required columns are missing", {
  
  data <- data.frame(
    indicator_id = 1,
    start_date = "2026-01-01"
  )
  
  expect_error(
    clean_data_types(data),
    "Missing required columns"
  )
})

test_that("clean_data_types errors when input is not a data frame", {
  
  expect_error(
    clean_data_types(c(1, 2, 3)),
    "`data` must be a data frame"
  )
})

test_that("clean_data_types converts invalid numeric values to NA", {
  
  data <- data.frame(
    indicator_id = "1",
    start_date = "2026-01-01",
    end_date = "2026-12-31",
    numerator = "not_numeric",
    denominator = "100",
    indicator_value = "10.5",
    lower_ci95 = "9.5",
    upper_ci95 = "11.5",
    imd_code = "2",
    aggregation_id = "3",
    age_group_code = "4",
    sex_code = "1",
    ethnicity_code = "5",
    creation_date = "2026-09-09 12:00:00",
    value_type_code = "2",
    source_code = "7",
    time_period_type = "Annual",
    combination_id = "1001",
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    result <- clean_data_types(data),
    "NAs introduced by coercion"
  )
  
  expect_true(
    is.na(result$numerator)
  )
})