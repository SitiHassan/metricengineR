# Helper data

create_test_pooling_metadata <- function() {
  
  data.frame(
    indicator_id = c(
      101L,
      102L,
      103L,
      104L
    ),
    requires_pooling = c(
      TRUE,
      FALSE,
      TRUE,
      FALSE
    )
  )
}


# Test 1: Indicators requiring pooling are returned

testthat::test_that(
  "get_indicators_requiring_pooling returns indicators marked for pooling",
  {
    
    metadata <- create_test_pooling_metadata()
    
    result <- get_indicators_requiring_pooling(
      metadata
    )
    
    testthat::expect_equal(
      result,
      c(101L, 103L)
    )
  }
)


# Test 2: Indicators not requiring pooling are excluded

testthat::test_that(
  "get_indicators_requiring_pooling excludes indicators not requiring pooling",
  {
    
    metadata <- create_test_pooling_metadata()
    
    result <- get_indicators_requiring_pooling(
      metadata
    )
    
    testthat::expect_false(
      102L %in% result
    )
    
    testthat::expect_false(
      104L %in% result
    )
  }
)


# Test 3: Duplicate indicator IDs are returned once

testthat::test_that(
  "get_indicators_requiring_pooling returns distinct indicator IDs",
  {
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        101L,
        102L
      ),
      requires_pooling = c(
        TRUE,
        TRUE,
        FALSE
      )
    )
    
    result <- get_indicators_requiring_pooling(
      metadata
    )
    
    testthat::expect_equal(
      result,
      101L
    )
  }
)


# Test 4: No indicators requiring pooling returns an empty vector

testthat::test_that(
  "get_indicators_requiring_pooling returns empty vector when pooling is not required",
  {
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        102L,
        103L
      ),
      requires_pooling = c(
        FALSE,
        FALSE,
        FALSE
      )
    )
    
    result <- get_indicators_requiring_pooling(
      metadata
    )
    
    testthat::expect_length(
      result,
      0L
    )
  }
)


# Test 5: All indicators requiring pooling are returned

testthat::test_that(
  "get_indicators_requiring_pooling returns all indicators when all require pooling",
  {
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        102L,
        103L
      ),
      requires_pooling = c(
        TRUE,
        TRUE,
        TRUE
      )
    )
    
    result <- get_indicators_requiring_pooling(
      metadata
    )
    
    testthat::expect_equal(
      result,
      c(101L, 102L, 103L)
    )
  }
)


# Test 6: Custom column names can be used

testthat::test_that(
  "get_indicators_requiring_pooling accepts custom column names",
  {
    
    metadata <- data.frame(
      metric_id = c(
        101L,
        102L,
        103L
      ),
      pooled_required = c(
        TRUE,
        FALSE,
        TRUE
      )
    )
    
    result <- get_indicators_requiring_pooling(
      metadata,
      indicator_col = "metric_id",
      pooling_col = "pooled_required"
    )
    
    testthat::expect_equal(
      result,
      c(101L, 103L)
    )
  }
)


# Test 7: Non-data-frame metadata is rejected

testthat::test_that(
  "get_indicators_requiring_pooling rejects non-data-frame metadata",
  {
    
    testthat::expect_error(
      get_indicators_requiring_pooling(
        c(1, 2, 3)
      ),
      "`metadata` must be a data frame"
    )
  }
)


# Test 8: Missing indicator column is reported

testthat::test_that(
  "get_indicators_requiring_pooling reports missing indicator column",
  {
    
    metadata <- data.frame(
      requires_pooling = c(
        TRUE,
        FALSE
      )
    )
    
    testthat::expect_error(
      get_indicators_requiring_pooling(
        metadata
      ),
      "Columns not found in `metadata`: indicator_id"
    )
  }
)


# Test 9: Missing pooling column is reported

testthat::test_that(
  "get_indicators_requiring_pooling reports missing pooling column",
  {
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        102L
      )
    )
    
    testthat::expect_error(
      get_indicators_requiring_pooling(
        metadata
      ),
      "Columns not found in `metadata`: requires_pooling"
    )
  }
)


# Test 10: Pooling column must be logical

testthat::test_that(
  "get_indicators_requiring_pooling requires logical pooling values",
  {
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        102L
      ),
      requires_pooling = c(
        "Yes",
        "No"
      )
    )
    
    testthat::expect_error(
      get_indicators_requiring_pooling(
        metadata
      ),
      "`requires_pooling` must contain TRUE/FALSE values"
    )
  }
)