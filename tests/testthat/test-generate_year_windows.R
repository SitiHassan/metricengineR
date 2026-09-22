# Test 1: Generates expected 3-year rolling windows
testthat::test_that(
  "generate_year_windows creates expected 3-year windows",
  {
    
    result <- generate_year_windows(
      min_year = 2014,
      max_year = 2020,
      span_years = 3
    )
    
    expected <- data.frame(
      from = 2014:2018,
      to = 2016:2020,
      k = rep(3L, 5)
    )
    
    testthat::expect_equal(
      result,
      expected
    )
  }
)


# Test 2: Generates expected 5-year rolling windows
testthat::test_that(
  "generate_year_windows creates expected 5-year windows",
  {
    
    result <- generate_year_windows(
      min_year = 2014,
      max_year = 2020,
      span_years = 5
    )
    
    expected <- data.frame(
      from = 2014:2016,
      to = 2018:2020,
      k = rep(5L, 3)
    )
    
    testthat::expect_equal(
      result,
      expected
    )
  }
)


# Test 3: span_years = 1 creates one row per year
testthat::test_that(
  "generate_year_windows supports one-year windows",
  {
    
    result <- generate_year_windows(
      min_year = 2018,
      max_year = 2020,
      span_years = 1
    )
    
    expected <- data.frame(
      from = 2018:2020,
      to = 2018:2020,
      k = rep(1L, 3)
    )
    
    testthat::expect_equal(
      result,
      expected
    )
  }
)


# Test 4: Exact span length creates one window
testthat::test_that(
  "generate_year_windows returns one window when range matches span",
  {
    
    result <- generate_year_windows(
      min_year = 2018,
      max_year = 2020,
      span_years = 3
    )
    
    expected <- data.frame(
      from = 2018L,
      to = 2020L,
      k = 3L
    )
    
    testthat::expect_equal(
      result,
      expected
    )
  }
)


# Test 5: Range shorter than span returns empty result
testthat::test_that(
  "generate_year_windows returns empty data frame when range is too short",
  {
    
    result <- generate_year_windows(
      min_year = 2019,
      max_year = 2020,
      span_years = 3
    )
    
    testthat::expect_equal(
      nrow(result),
      0
    )
    
    testthat::expect_equal(
      names(result),
      c("from", "to", "k")
    )
    
    testthat::expect_type(
      result$from,
      "integer"
    )
    
    testthat::expect_type(
      result$to,
      "integer"
    )
    
    testthat::expect_type(
      result$k,
      "integer"
    )
  }
)


# Test 6: min_year must be valid
testthat::test_that(
  "generate_year_windows validates min_year",
  {
    
    testthat::expect_error(
      generate_year_windows(
        min_year = "2014",
        max_year = 2020
      ),
      "`min_year` must be a single whole number"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014.5,
        max_year = 2020
      ),
      "`min_year` must be a single whole number"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = NA,
        max_year = 2020
      ),
      "`min_year` must be a single whole number"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = c(2014, 2015),
        max_year = 2020
      ),
      "`min_year` must be a single whole number"
    )
  }
)


# Test 7: max_year must be valid
testthat::test_that(
  "generate_year_windows validates max_year",
  {
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = "2020"
      ),
      "`max_year` must be a single whole number"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020.5
      ),
      "`max_year` must be a single whole number"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = NA
      ),
      "`max_year` must be a single whole number"
    )
  }
)


# Test 8: span_years must be valid
testthat::test_that(
  "generate_year_windows validates span_years",
  {
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020,
        span_years = 0
      ),
      "`span_years` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020,
        span_years = -1
      ),
      "`span_years` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020,
        span_years = 3.5
      ),
      "`span_years` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020,
        span_years = NA
      ),
      "`span_years` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2014,
        max_year = 2020,
        span_years = "3"
      ),
      "`span_years` must be a whole number greater than or equal to 1"
    )
  }
)


# Test 9: min_year cannot be greater than max_year
testthat::test_that(
  "generate_year_windows rejects min_year greater than max_year",
  {
    
    testthat::expect_error(
      generate_year_windows(
        min_year = 2020,
        max_year = 2014,
        span_years = 3
      ),
      "`min_year` cannot be greater than `max_year`"
    )
  }
)


# Test 10: Output columns are integers
testthat::test_that(
  "generate_year_windows returns integer output columns",
  {
    
    result <- generate_year_windows(
      min_year = 2014,
      max_year = 2020,
      span_years = 3
    )
    
    testthat::expect_type(
      result$from,
      "integer"
    )
    
    testthat::expect_type(
      result$to,
      "integer"
    )
    
    testthat::expect_type(
      result$k,
      "integer"
    )
  }
)