# Helper data 

create_test_missing_values_data <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    numerator = c(10, NA, 30, 40),
    denominator = c(100, NA, 300, 400),
    indicator_value = c(10, 20, NA, 40),
    lower_ci95 = c(8, NA, 25, 35),
    upper_ci95 = c(12, NA, 35, 45),
    source_code = c(1L, NA, 1L, 1L),
    aggregation_id = c(151L, 151L, 151L, 151L)
  )
}


create_test_missing_values_metadata <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    status_code = c(1L, 2L, 3L, 1L),
    precalculated = c("No", "No", "No", "Yes")
  )
}

# Test 1: Missing values are identified 

testthat::test_that(
  "check_missing_values identifies missing values",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
  }
)


# Test 2: Ignored columns do not trigger missing values 

testthat::test_that(
  "check_missing_values ignores default excluded columns",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    # Remove the missing source_code so indicator 102 only has missing
    # values in columns ignored by default
    df$source_code[df$indicator_id == 102L] <- 1L
    
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 3: Specific columns can be checked 

testthat::test_that(
  "check_missing_values checks specified columns",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    result <- check_missing_values(
      df = df,
      metadata = metadata,
      cols = c(
        "numerator",
        "denominator"
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
  }
)


# Test 4: Status codes control which indicators are checked 

testthat::test_that(
  "check_missing_values applies status code filtering",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    result <- check_missing_values(
      df = df,
      metadata = metadata,
      status_codes = 1L
    )
    
    # Indicator 102 has status 2, so it should not be checked
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 5: Default status codes include 1 and 2 

testthat::test_that(
  "check_missing_values includes status codes 1 and 2 by default",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_true(
      102L %in% result$indicator_id
    )
  }
)


# Test 6: Precalculated indicators are excluded 

testthat::test_that(
  "check_missing_values excludes precalculated indicators",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    # Indicator 104 is precalculated, so create a missing value
    df$source_code[df$indicator_id == 104L] <- NA
    
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_false(
      104L %in% result$indicator_id
    )
  }
)


# Test 7: Indicators outside selected statuses are excluded 

testthat::test_that(
  "check_missing_values excludes indicators outside selected statuses",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    # Indicator 103 has status 3 and already has missing indicator_value
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_false(
      103L %in% result$indicator_id
    )
  }
)


# Test 8: No missing values returns empty result 

testthat::test_that(
  "check_missing_values returns empty data frame when no missing values exist",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    df$source_code <- 1L
    df$indicator_value <- c(10, 20, 30, 40)
    
    result <- check_missing_values(
      df = df,
      metadata = metadata
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 9: PASS message is returned 

testthat::test_that(
  "check_missing_values reports when no missing values are found",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    df$source_code <- 1L
    df$indicator_value <- c(10, 20, 30, 40)
    
    testthat::expect_message(
      check_missing_values(
        df = df,
        metadata = metadata
      ),
      "PASS: No rows with missing values"
    )
  }
)


# Test 10: Warning message is returned 

testthat::test_that(
  "check_missing_values reports missing values",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    testthat::expect_message(
      check_missing_values(
        df = df,
        metadata = metadata
      ),
      "WARNING: Found 1 row"
    )
  }
)


# Test 11: Non-data-frame df is rejected 

testthat::test_that(
  "check_missing_values rejects non-data-frame df",
  {
    
    metadata <- create_test_missing_values_metadata()
    
    testthat::expect_error(
      check_missing_values(
        df = c(1, 2, 3),
        metadata = metadata
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 12: Non-data-frame metadata is rejected 

testthat::test_that(
  "check_missing_values rejects non-data-frame metadata",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = c(1, 2, 3)
      ),
      "`metadata` must be a data frame"
    )
  }
)


# Test 13: Missing metadata columns are reported 

testthat::test_that(
  "check_missing_values reports missing metadata columns",
  {
    
    df <- create_test_missing_values_data()
    
    metadata <- create_test_missing_values_metadata() |>
      dplyr::select(
        -precalculated
      )
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata
      ),
      "Missing required metadata columns: precalculated"
    )
  }
)


# Test 14: Missing indicator_id in df is reported 

testthat::test_that(
  "check_missing_values requires indicator_id in df",
  {
    
    df <- create_test_missing_values_data() |>
      dplyr::select(
        -indicator_id
      )
    
    metadata <- create_test_missing_values_metadata()
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata
      ),
      "Missing required column: indicator_id"
    )
  }
)


# Test 15: Requested columns must exist 

testthat::test_that(
  "check_missing_values rejects requested columns that do not exist",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata,
        cols = c(
          "source_code",
          "missing_column"
        )
      ),
      "Columns not found in `df`: missing_column"
    )
  }
)


# Test 16: show_n must be valid 

testthat::test_that(
  "check_missing_values validates show_n",
  {
    
    df <- create_test_missing_values_data()
    metadata <- create_test_missing_values_metadata()
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata,
        show_n = 0
      ),
      "`show_n` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata,
        show_n = 2.5
      ),
      "`show_n` must be a whole number greater than or equal to 1"
    )
    
    testthat::expect_error(
      check_missing_values(
        df = df,
        metadata = metadata,
        show_n = NA
      ),
      "`show_n` must be a whole number greater than or equal to 1"
    )
  }
)