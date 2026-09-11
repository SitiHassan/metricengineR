# Test 1: Basic percentage calculation without confidence intervals
# Checks that eligible rows are calculated correctly and CI columns remain NA.
testthat::test_that("calc_percentage calculates percentages without confidence intervals", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(50, 30),
    denominator = c(100, 50),
    value_multiplier = c(100, 100)
  )
  
  result <- calc_percentage(df)
  
  testthat::expect_equal(
    result$value,
    c(50, 60)
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})


# Test 2: Percentages above 100% are allowed when CIs are not required
# This is important for measures such as actual against plan or growth.
testthat::test_that("calc_percentage allows percentages above 100 without confidence intervals", {
  
  df <- data.frame(
    value_type_code = 2L,
    numerator = 120,
    denominator = 100,
    value_multiplier = 100
  )
  
  result <- calc_percentage(
    df,
    confidence_intervals_required = FALSE
  )
  
  testthat::expect_equal(
    result$value,
    120
  )
})


# Test 3: Only percentage value type rows are returned
# Checks that calc_percentage only processes value_type_code 2.
testthat::test_that("calc_percentage filters to value type code 2", {
  
  df <- data.frame(
    value_type_code = c(2L, 9L, 10L, 2L),
    numerator = c(50, 20, 30, 40),
    denominator = c(100, 100, 100, 80),
    value_multiplier = c(100, 100, 100, 100)
  )
  
  result <- calc_percentage(df)
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_true(
    all(result$value_type_code == 2L)
  )
})


# Test 4: Ineligible rows are retained with NA calculated values
# Checks missing numerator, missing denominator, zero denominator,
# and missing multiplier.
testthat::test_that("calc_percentage retains ineligible rows with NA values", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L, 2L, 2L, 2L),
    numerator = c(50, 20, NA, 30, 40),
    denominator = c(100, 0, 100, NA, 80),
    value_multiplier = c(100, 100, 100, 100, NA)
  )
  
  result <- calc_percentage(df)
  
  testthat::expect_equal(
    nrow(result),
    5
  )
  
  testthat::expect_equal(
    result$value[1],
    50
  )
  
  testthat::expect_true(
    all(is.na(result$value[2:5]))
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})


# Test 5: Custom column names work
# Checks that the function is not dependent on the default column names.
testthat::test_that("calc_percentage works with custom column names", {
  
  df <- data.frame(
    type_code = c(2L, 2L),
    events = c(25, 75),
    population = c(50, 100),
    multiplier = c(100, 100)
  )
  
  result <- calc_percentage(
    df,
    value_type_code_col = "type_code",
    numerator_col = "events",
    denominator_col = "population",
    value_multiplier_col = "multiplier"
  )
  
  testthat::expect_equal(
    result$value,
    c(50, 75)
  )
})


# Test 6: Valid proportions calculate confidence intervals
# Checks the confidence interval branch and confirms that values and
# 95% confidence intervals are returned.
testthat::test_that("calc_percentage calculates proportion confidence intervals", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(50, 30),
    denominator = c(100, 50),
    value_multiplier = c(100, 100)
  )
  
  result <- calc_percentage(
    df,
    confidence_intervals_required = TRUE
  )
  
  testthat::expect_equal(
    result$value,
    c(50, 60)
  )
  
  testthat::expect_true(
    all(!is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(!is.na(result$uppercl))
  )
  
  testthat::expect_true(
    all(result$lowercl <= result$value)
  )
  
  testthat::expect_true(
    all(result$uppercl >= result$value)
  )
})


# Test 7: Ineligible rows are still retained when CIs are required
# Checks that rows such as denominator = 0 are not removed from the output.
testthat::test_that("calc_percentage retains ineligible rows when confidence intervals are required", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(50, 20),
    denominator = c(100, 0),
    value_multiplier = c(100, 100)
  )
  
  result <- calc_percentage(
    df,
    confidence_intervals_required = TRUE
  )
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_false(
    is.na(result$value[1])
  )
  
  testthat::expect_true(
    is.na(result$value[2])
  )
  
  testthat::expect_true(
    is.na(result$lowercl[2])
  )
  
  testthat::expect_true(
    is.na(result$uppercl[2])
  )
})


# Test 8: Numerator greater than denominator is rejected for proportion CIs
# The same value may be valid as a percentage without CIs but is not
# a valid genuine proportion.
testthat::test_that("calc_percentage rejects numerator greater than denominator for proportion CIs", {
  
  df <- data.frame(
    value_type_code = 2L,
    numerator = 120,
    denominator = 100,
    value_multiplier = 100
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = TRUE
    ),
    "Invalid proportion: numerator must be between 0 and denominator."
  )
})


# Test 9: Negative numerator is rejected for proportion CIs
# Genuine proportions cannot have a negative numerator.
testthat::test_that("calc_percentage rejects negative numerator for proportion CIs", {
  
  df <- data.frame(
    value_type_code = 2L,
    numerator = -10,
    denominator = 100,
    value_multiplier = 100
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = TRUE
    ),
    "Invalid proportion: numerator must be between 0 and denominator."
  )
})


# Test 10: Unsupported confidence interval methods are rejected
# calc_percentage currently only supports proportion confidence intervals.
testthat::test_that("calc_percentage rejects unsupported confidence interval methods", {
  
  df <- data.frame(
    value_type_code = 2L,
    numerator = 50,
    denominator = 100,
    value_multiplier = 100
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = TRUE,
      ci_method = "rate"
    ),
    "`ci_method` must be \"proportion\""
  )
})


# Test 11: Multiple multipliers are rejected when calculating proportion CIs
# phe_proportion uses one multiplier for a calculation, so eligible rows
# must use the same multiplier.
testthat::test_that("calc_percentage rejects multiple value multipliers for confidence intervals", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(50, 30),
    denominator = c(100, 50),
    value_multiplier = c(100, 1000)
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = TRUE
    ),
    "More than one value multiplier found for percentage calculations."
  )
})


# Test 12: Non-data-frame inputs are rejected
# Checks the basic input validation.
testthat::test_that("calc_percentage rejects non-data-frame input", {
  
  testthat::expect_error(
    calc_percentage(c(1, 2, 3)),
    "`df` must be a data frame."
  )
})


# Test 13: Missing required columns are reported
# Checks that the function fails clearly when required input columns are absent.
testthat::test_that("calc_percentage errors when required columns are missing", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(50, 30)
  )
  
  testthat::expect_error(
    calc_percentage(df),
    "Missing required columns: denominator, value_multiplier"
  )
})


# Test 14: confidence_intervals_required must be one TRUE or FALSE
# Checks character, multiple logical values, and NA.
testthat::test_that("calc_percentage validates confidence_intervals_required", {
  
  df <- data.frame(
    value_type_code = 2L,
    numerator = 50,
    denominator = 100,
    value_multiplier = 100
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = "TRUE"
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = c(TRUE, FALSE)
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
  
  testthat::expect_error(
    calc_percentage(
      df,
      confidence_intervals_required = NA
    ),
    "`confidence_intervals_required` must be TRUE or FALSE."
  )
})


# Test 15: All ineligible rows can be returned when CIs are requested
# Checks that phe_proportion is not required when there are no eligible rows
# and all calculated values remain NA.
testthat::test_that("calc_percentage handles no eligible rows with confidence intervals", {
  
  df <- data.frame(
    value_type_code = c(2L, 2L),
    numerator = c(20, NA),
    denominator = c(0, 100),
    value_multiplier = c(100, 100)
  )
  
  result <- calc_percentage(
    df,
    confidence_intervals_required = TRUE
  )
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_true(
    all(is.na(result$value))
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})