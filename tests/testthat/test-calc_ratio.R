# Test 1: Basic ratio calculation without confidence intervals
# Checks that eligible rows calculate correctly and CIs remain NA.
testthat::test_that("calculate_ratio calculates ratios without confidence intervals", {
  
  df <- data.frame(
    value_type_code = c(7L, 7L),
    numerator = c(50, 25),
    denominator = c(1000, 500),
    value_multiplier = c(1, 1)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(
    result$value,
    c(0.05, 0.05)
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})


# Test 2: Only ratio rows are returned
# Checks that other value types are excluded.
testthat::test_that("calculate_ratio filters to ratio value type", {
  
  df <- data.frame(
    value_type_code = c(7L, 2L, 1L, 7L),
    numerator = c(50, 20, 30, 40),
    denominator = c(1000, 100, 100, 800),
    value_multiplier = c(1, 100, 1, 1)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_true(
    all(result$value_type_code == 7L)
  )
})


# Test 3: Ineligible rows are retained with NA values
# Checks missing numerator, zero denominator, missing denominator,
# and missing value multiplier.
testthat::test_that("calculate_ratio retains ineligible rows with NA values", {
  
  df <- data.frame(
    value_type_code = c(7L, 7L, 7L, 7L, 7L),
    numerator = c(50, NA, 20, 30, 40),
    denominator = c(1000, 1000, 0, NA, 800),
    value_multiplier = c(1, 1, 1, 1, NA)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(
    nrow(result),
    5
  )
  
  testthat::expect_equal(
    result$value[1],
    0.05
  )
  
  testthat::expect_true(
    all(is.na(result$value[2:5]))
  )
})


# Test 4: Negative numerators/denominators are not rejected
testthat::test_that("calculate_ratio does not reject negative numerators nor negative denominators", {
  
  df <- data.frame(
    value_type_code = c(7L, 7L),
    numerator = c(-10, 10),
    denominator = c(1000, -1000),
    value_multiplier = c(1, 1)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(
    result$value,
    c(-0.01, -0.01)
  )
  
})


# Test 5: Custom column names work
# Checks that the function is not dependent on default column names.
testthat::test_that("calculate_ratio works with custom column names", {
  
  df <- data.frame(
    type_code = c(7L, 7L),
    events = c(10, 20),
    population = c(1000, 2000),
    multiplier = c(1, 1)
  )
  
  result <- calculate_ratio(
    df,
    value_type_code_col = "type_code",
    numerator_col = "events",
    denominator_col = "population",
    value_multiplier_col = "multiplier"
  )
  
  testthat::expect_equal(
    result$value,
    c(0.01, 0.01)
  )
})


# Test 6: Confidence intervals are not currently implemented
# Checks that requesting CIs gives the intended error.
testthat::test_that("calculate_ratio stops when confidence intervals are required", {
  
  df <- data.frame(
    value_type_code = 7L,
    numerator = 25,
    denominator = 100,
    value_multiplier = 1
  )
  
  testthat::expect_error(
    calculate_ratio(
      df,
      confidence_intervals_required = TRUE
    ),
    "Confidence intervals for ratios have not yet been implemented."
  )
})



# Test 7: Non-data-frame inputs are rejected
testthat::test_that("calculate_ratio rejects non-data-frame input", {
  
  testthat::expect_error(
    calculate_ratio(c(1, 2, 3)),
    "`df` must be a data frame"
  )
})


# Test 8: Missing required columns are reported
testthat::test_that("calculate_ratio errors when required columns are missing", {
  
  df <- data.frame(
    value_type_code = 7L,
    numerator = 50
  )
  
  testthat::expect_error(
    calculate_ratio(df),
    "Missing required columns: denominator, value_multiplier"
  )
})


# Test 9: confidence_intervals_required must be one TRUE or FALSE
testthat::test_that("calculate_ratio validates confidence_intervals_required", {
  
  df <- data.frame(
    value_type_code = 7L,
    numerator = 50,
    denominator = 10000,
    value_multiplier = 100000
  )
  
  testthat::expect_error(
    calculate_ratio(
      df,
      confidence_intervals_required = "TRUE"
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    calculate_ratio(
      df,
      confidence_intervals_required = c(TRUE, FALSE)
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    calculate_ratio(
      df,
      confidence_intervals_required = NA
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
})

# Test 10: Value multiplier is applied
testthat::test_that("calculate_ratio applies the value multiplier", {
  
  df <- data.frame(
    value_type_code = c(7L, 7L),
    numerator = c(25, 25),
    denominator = c(100, 100),
    value_multiplier = c(1, 100)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(
    result$value,
    c(0.25, 25)
  )
})

# Test 11: No ratio rows returns an empty data frame
testthat::test_that("calculate_ratio handles no ratio rows", {
  
  df <- data.frame(
    value_type_code = c(1L, 2L),
    numerator = c(10, 20),
    denominator = c(100, 200),
    value_multiplier = c(1, 100)
  )
  
  result <- calculate_ratio(df)
  
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true(
    all(c("value", "lowercl", "uppercl") %in% names(result))
  )
})
