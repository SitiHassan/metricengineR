# Test 1: Correct combination IDs are created
testthat::test_that(
  "create_inequality_combination_id creates expected combination IDs",
  {
    
    df <- data.frame(
      imd_code = c(1L, 1L, 999L, 999L),
      ethnicity_code = c(2L, 999L, 2L, 999L)
    )
    
    result <- create_inequality_combination_id(df)
    
    testthat::expect_equal(
      result$combination_id,
      c(1L, 2L, 3L, 4L)
    )
  }
)


# Test 2: combination_id is returned as an integer
testthat::test_that(
  "create_inequality_combination_id returns an integer combination ID",
  {
    
    df <- data.frame(
      imd_code = c(1L, 999L),
      ethnicity_code = c(999L, 999L)
    )
    
    result <- create_inequality_combination_id(df)
    
    testthat::expect_type(
      result$combination_id,
      "integer"
    )
  }
)


# Test 3: Original rows and columns are retained
testthat::test_that(
  "create_inequality_combination_id retains original data",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      imd_code = c(1L, 999L),
      ethnicity_code = c(999L, 999L),
      numerator = c(100, 200)
    )
    
    result <- create_inequality_combination_id(df)
    
    testthat::expect_equal(
      nrow(result),
      nrow(df)
    )
    
    testthat::expect_equal(
      result$indicator_id,
      df$indicator_id
    )
    
    testthat::expect_equal(
      result$numerator,
      df$numerator
    )
    
    testthat::expect_true(
      "combination_id" %in% names(result)
    )
  }
)


# Test 4: Custom column names work
testthat::test_that(
  "create_inequality_combination_id works with custom column names",
  {
    
    df <- data.frame(
      deprivation = c(1L, 1L, 999L, 999L),
      ethnicity = c(2L, 999L, 2L, 999L)
    )
    
    result <- create_inequality_combination_id(
      df,
      imd_code_col = "deprivation",
      ethnicity_code_col = "ethnicity"
    )
    
    testthat::expect_equal(
      result$combination_id,
      c(1L, 2L, 3L, 4L)
    )
  }
)


# Test 5: Custom overall code works
testthat::test_that(
  "create_inequality_combination_id supports a custom overall code",
  {
    
    df <- data.frame(
      imd_code = c(1L, 1L, 0L, 0L),
      ethnicity_code = c(2L, 0L, 2L, 0L)
    )
    
    result <- create_inequality_combination_id(
      df,
      overall_code = 0L
    )
    
    testthat::expect_equal(
      result$combination_id,
      c(1L, 2L, 3L, 4L)
    )
  }
)


# Test 6: Missing IMD or ethnicity values return NA
testthat::test_that(
  "create_inequality_combination_id returns NA for missing codes",
  {
    
    df <- data.frame(
      imd_code = c(NA_integer_, 1L, 999L),
      ethnicity_code = c(2L, NA_integer_, NA_integer_)
    )
    
    result <- create_inequality_combination_id(df)
    
    testthat::expect_true(
      all(is.na(result$combination_id))
    )
  }
)


# Test 7: Non-data-frame input is rejected
testthat::test_that(
  "create_inequality_combination_id rejects non-data-frame input",
  {
    
    testthat::expect_error(
      create_inequality_combination_id(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 8: Missing IMD column is reported
testthat::test_that(
  "create_inequality_combination_id reports missing IMD column",
  {
    
    df <- data.frame(
      ethnicity_code = c(1L, 999L)
    )
    
    testthat::expect_error(
      create_inequality_combination_id(df),
      "Missing required columns: imd_code"
    )
  }
)


# Test 9: Missing ethnicity column is reported
testthat::test_that(
  "create_inequality_combination_id reports missing ethnicity column",
  {
    
    df <- data.frame(
      imd_code = c(1L, 999L)
    )
    
    testthat::expect_error(
      create_inequality_combination_id(df),
      "Missing required columns: ethnicity_code"
    )
  }
)


# Test 10: Both missing required columns are reported
testthat::test_that(
  "create_inequality_combination_id reports all missing required columns",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L)
    )
    
    testthat::expect_error(
      create_inequality_combination_id(df),
      "Missing required columns: imd_code, ethnicity_code"
    )
  }
)


# Test 11: Existing combination_id is replaced
testthat::test_that(
  "create_inequality_combination_id replaces an existing combination ID",
  {
    
    df <- data.frame(
      imd_code = c(1L, 999L),
      ethnicity_code = c(999L, 999L),
      combination_id = c(99L, 99L)
    )
    
    result <- create_inequality_combination_id(df)
    
    testthat::expect_equal(
      result$combination_id,
      c(2L, 4L)
    )
  }
)