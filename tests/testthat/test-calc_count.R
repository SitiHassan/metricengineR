# Test 1: Basic count assignment
# Checks that count values are copied into `value`
# and confidence interval columns are NA.
testthat::test_that("calc_count assigns count values correctly", {
  
  df <- data.frame(
    value_type_code = c(1L, 1L),
    numerator = c(25, 40)
  )
  
  result <- calc_count(df)
  
  testthat::expect_equal(
    result$value,
    c(25, 40)
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})


# Test 2: Only count value type rows are returned
# Checks that only rows with value_type_code 1 are processed.
testthat::test_that("calc_count filters to value type code 1", {
  
  df <- data.frame(
    value_type_code = c(1L, 2L, 9L, 1L),
    numerator = c(10, 20, 30, 40)
  )
  
  result <- calc_count(df)
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_true(
    all(result$value_type_code == 1L)
  )
})


# Test 3: Missing count values are retained as NA
# Checks that missing counts are not replaced with zero.
testthat::test_that("calc_count preserves missing count values", {
  
  df <- data.frame(
    value_type_code = c(1L, 1L),
    numerator = c(25, NA)
  )
  
  result <- calc_count(df)
  
  testthat::expect_equal(
    result$value[1],
    25
  )
  
  testthat::expect_true(
    is.na(result$value[2])
  )
})


# Test 4: Custom column names work
# Checks that the function works when the count and value type
# columns use different names.
testthat::test_that("calc_count works with custom column names", {
  
  df <- data.frame(
    type_code = c(1L, 1L),
    count_value = c(15, 30)
  )
  
  result <- calc_count(
    df,
    value_type_code_col = "type_code",
    count_col = "count_value"
  )
  
  testthat::expect_equal(
    result$value,
    c(15, 30)
  )
})


# Test 5: Non-data-frame inputs are rejected
# Checks the basic input validation.
testthat::test_that("calc_count rejects non-data-frame input", {
  
  testthat::expect_error(
    calc_count(c(1, 2, 3)),
    "`df` must be a data frame."
  )
})


# Test 6: Missing required columns are reported
# Checks that the function fails clearly when required columns are absent.
testthat::test_that("calc_count errors when required columns are missing", {
  
  df <- data.frame(
    value_type_code = c(1L, 1L)
  )
  
  testthat::expect_error(
    calc_count(df),
    "Missing required columns: numerator"
  )
})


# Test 7: confidence_intervals_required must be TRUE or FALSE
# Checks invalid character, multiple logical values, and NA.
testthat::test_that("calc_count validates confidence_intervals_required", {
  
  df <- data.frame(
    value_type_code = 1L,
    numerator = 25
  )
  
  testthat::expect_error(
    calc_count(
      df,
      confidence_intervals_required = "TRUE"
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  testthat::expect_error(
    calc_count(
      df,
      confidence_intervals_required = c(TRUE, FALSE)
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  testthat::expect_error(
    calc_count(
      df,
      confidence_intervals_required = NA
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
})


# Test 8: Confidence intervals are not currently implemented
# Checks that requesting CIs gives the intended error.
testthat::test_that("calc_count stops when confidence intervals are required", {
  
  df <- data.frame(
    value_type_code = 1L,
    numerator = 25
  )
  
  testthat::expect_error(
    calc_count(
      df,
      confidence_intervals_required = TRUE
    ),
    "Confidence intervals for counts have not yet been implemented."
  )
})