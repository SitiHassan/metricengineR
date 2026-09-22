# Test 1: Returns the most common value 
testthat::test_that(
  "most_common_value returns the most frequent value",
  {
    
    x <- c(
      "03-31",
      "03-31",
      "12-31",
      "03-31"
    )
    
    result <- most_common_value(x)
    
    testthat::expect_equal(
      result,
      "03-31"
    )
  }
)


# Test 2: Missing values are ignored 

testthat::test_that(
  "most_common_value ignores missing values",
  {
    
    x <- c(
      "03-31",
      NA,
      "03-31",
      "12-31",
      NA
    )
    
    result <- most_common_value(x)
    
    testthat::expect_equal(
      result,
      "03-31"
    )
  }
)


# Test 3: All missing values return NA 

testthat::test_that(
  "most_common_value returns NA when all values are missing",
  {
    
    x <- c(
      NA_character_,
      NA_character_
    )
    
    result <- most_common_value(x)
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 4: Empty vector returns NA 

testthat::test_that(
  "most_common_value returns NA for an empty vector",
  {
    
    result <- most_common_value(
      character(0)
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 5: Single value is returned 

testthat::test_that(
  "most_common_value handles a single value",
  {
    
    result <- most_common_value(
      "12-31"
    )
    
    testthat::expect_equal(
      result,
      "12-31"
    )
  }
)


# Test 6: Numeric values can be handled 

testthat::test_that(
  "most_common_value handles numeric values",
  {
    
    x <- c(
      1,
      2,
      2,
      3,
      2
    )
    
    result <- most_common_value(x)
    
    testthat::expect_equal(
      result,
      "2"
    )
  }
)