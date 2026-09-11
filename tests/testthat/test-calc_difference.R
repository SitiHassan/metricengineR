# Test the calculation 
testthat::test_that("calc_difference calculates the correct difference", {
  
  df <- data.frame(
    value_type_code = c(10L, 11L),
    numerator = c(70, 120),
    denominator = c(50, 100)
  )
  
  result <- calc_difference(df)
  
  expect_equal(
    result$value,
    c(20, 20)
  )
  
  expect_true(
    all(is.na(result$lowercl))
  )
  
  expect_true(
    all(is.na(result$uppercl))
  )
})

# Test that it only keeps the selected value type codes
testthat::test_that("calc_difference filters to selected value type code", {
  
  df <- data.frame(
    value_type_code = c(10L, 11L, 12L),
    numerator = c(70, 120, 200),
    denominator = c(50, 100, 150)
  )
  
  result <- calc_difference(df)
  
  expect_equal(
    nrow(result),
    2
  )
  
  expect_setequal(
    result$value_type_code,
    c(10L, 11L)
  )
})

# Test custom columns 
testthat::test_that("calc_difference works with custom column names", {
  
  df <- data.frame(
    type_code = c(10L, 11L),
    local_value = c(80, 60),
    national_value = c(75, 70)
  )
  
  result <- calc_difference(
    df,
    value_type_code_col = "type_code",
    minuend_col = "local_value",
    subtrahend_col = "national_value"
  )
  
  expect_equal(
    result$value,
    c(5, -10)
  )
  
  expect_true(
    all(is.na(result$lowercl))
  )
  
  expect_true(
    all(is.na(result$uppercl))
  )
})

# Test invalid input
testthat::test_that("calc_difference rejects non-data-frame input", {
  
  expect_error(
    calc_difference(c(1, 2, 3)),
    "`df` must be a data frame"
  )
})

# Test missing columns
testthat::test_that("calc_difference errors when required columns are missing", {
  
  df <- data.frame(
    value_type_code = c(10L, 11L),
    numerator = c(70, 120)
  )
  
  expect_error(
    calc_difference(df),
    "Missing required columns: denominator"
  )
})

# Test negative-difference 
testthat::test_that("calc_difference preserves negative differences", {
  
  df <- data.frame(
    value_type_code = c(10L),
    numerator = 45,
    denominator = 50
  )
  
  result <- calc_difference(df)
  
  expect_equal(
    result$value,
    -5
  )
})