# Test the calculation 
testthat::test_that("calc_percentage_change calculates the correct change", {
  
  df <- data.frame(
    value_type_code = c(9L, 9L),
    numerator = c(70, 120),
    denominator = c(50, 100),
    value_multiplier = 100
  )
  
  result <- calc_percentage_change(df)
  
  expect_equal(
    result$value,
    c(40, 20)
  )
  
  expect_true(
    all(is.na(result$lowercl))
  )
  
  expect_true(
    all(is.na(result$uppercl))
  )
})

# Test it returns NA when old value is 0 or NA
testthat::test_that("calc_percentage_change returns NA when old value is zero or missing", {
  
  df <- data.frame(
    value_type_code = c(9L, 9L),
    numerator = c(70, 120),
    denominator = c(0, NA),
    value_multiplier = 100
  )
  
  result <- calc_percentage_change(df)
  
  expect_true(
    all(is.na(result$value))
  )
  
})

# Test it applies different value multiplier
testthat::test_that("calc_percentage_change applies the value multiplier", {
  
  df <- data.frame(
    value_type_code = 9L,
    numerator = 60,
    denominator = 50,
    value_multiplier = 1
  )
  
  result <- calc_percentage_change(df)
  
  expect_equal(
    result$value,
    0.2
  )
})

# Test what happens when there are no value type 9 rows
testthat::test_that("calc_percentage_change returns zero rows when value type 9 is absent", {
  
  df <- data.frame(
    value_type_code = c(10L, 11L),
    numerator = c(70, 120),
    denominator = c(50, 100),
    value_multiplier = 100
  )
  
  result <- calc_percentage_change(df)
  
  expect_equal(
    nrow(result),
    0
  )
})

# Test that it only keeps the selected value type codes
testthat::test_that("calc_percentage_change filters to value type code 9", {
  
  df <- data.frame(
    value_type_code = c(10L, 11L, 9L),
    numerator = c(70, 120, 30),
    denominator = c(50, 100, 70),
    value_multiplier = 100L
  )
  
  result <- calc_percentage_change(df)
  
  expect_equal(
    nrow(result),
    1
  )
  
  expect_setequal(
    result$value_type_code,
    c(9L)
  )
})

# Test custom columns 
testthat::test_that("calc_percentage_change works with custom column names", {
  
  df <- data.frame(
    type_code = c(9L, 9L),
    current_value = c(80, 60),
    plan_value = c(75, 70),
    multiplier = 100L
  )
  
  result <- calc_percentage_change(
    df,
    value_type_code_col = "type_code",
    new_value_col = "current_value",
    old_value_col = "plan_value",
    value_multiplier_col = "multiplier"
  )
  
  expect_equal(
    result$value,
    c(
      (80 - 75) / 75 * 100, 
      (60 - 70) / 70 * 100
      )
  )
  
  expect_true(
    all(is.na(result$lowercl))
  )
  
  expect_true(
    all(is.na(result$uppercl))
  )
})

# Test invalid input
testthat::test_that("calc_percentage_change rejects non-data-frame input", {
  
  expect_error(
    calc_percentage_change(c(1, 2, 3)),
    "`df` must be a data frame"
  )
})

testthat::test_that("calc_percentage_change rejects non-logical or invalid confidence intervals required input", {
  
  df <- data.frame(
    value_type_code = c(9L, 9L),
    numerator = c(70, 120),
    denominator = c(50, 100),
    value_multiplier = 100L
  )
  
  expect_error(
    calc_percentage_change(
      df, 
      confidence_intervals_required = "FALSE"
      ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  expect_error(
    calc_percentage_change(
      df,
      confidence_intervals_required = c(FALSE, TRUE)
      ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  expect_error(
    calc_percentage_change(
      df,
      confidence_intervals_required = NA
      ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
})

# Test it stops when confidence intervals are required
testthat::test_that("calc_percentage_change stops when confidence intervals are required", {
  
  df <- data.frame(
    value_type_code = c(9L, 9L),
    numerator = c(70, 120),
    denominator = c(50, 100),
    value_multiplier = 100L
  )
  
  expect_error(
    calc_percentage_change(df, confidence_intervals_required = TRUE),
    "Confidence intervals for percentage change have not yet been implemented."
  )
  
})

# Test missing columns
testthat::test_that("calc_percentage_change errors when required columns are missing", {
  
  df <- data.frame(
    value_type_code = c(9L, 9L),
    numerator = c(70, 120)
  )
  
  expect_error(
    calc_percentage_change(df),
    "Missing required columns: denominator, value_multiplier"
  )
})