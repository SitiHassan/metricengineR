test_that("normalise_indicator_ids returns NULL when NULL is supplied", {
  
  result <- normalise_indicator_ids(NULL)
  
  expect_null(result)
})


test_that("normalise_indicator_ids returns NULL for All", {
  
  expect_null(
    normalise_indicator_ids("All")
  )
  
  expect_null(
    normalise_indicator_ids("all")
  )
  
  expect_null(
    normalise_indicator_ids("ALL")
  )
})


test_that("normalise_indicator_ids returns NULL for *", {
  
  expect_null(
    normalise_indicator_ids("*")
  )
})


test_that("normalise_indicator_ids accepts numeric vectors", {
  
  result <- normalise_indicator_ids(
    c(10, 11, 12)
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
  
  expect_type(
    result,
    "double"
  )
})


test_that("normalise_indicator_ids converts numeric character vectors to numeric", {
  
  result <- normalise_indicator_ids(
    c("10", "11", "12")
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
  
  expect_type(
    result,
    "double"
  )
})


test_that("normalise_indicator_ids splits a comma-separated string", {
  
  result <- normalise_indicator_ids(
    "10, 11, 12"
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
})


test_that("normalise_indicator_ids removes surrounding whitespace", {
  
  result <- normalise_indicator_ids(
    c(" 10 ", " 11", "12 ")
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
})


test_that("normalise_indicator_ids removes whitespace from comma-separated IDs", {
  
  result <- normalise_indicator_ids(
    " 10,   11,  12 "
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
})


test_that("normalise_indicator_ids removes duplicate IDs", {
  
  result <- normalise_indicator_ids(
    c("10", "10", "11", "12", "12")
  )
  
  expect_equal(
    result,
    c(10, 11, 12)
  )
})


test_that("normalise_indicator_ids removes NA and blank values", {
  
  result <- normalise_indicator_ids(
    c("10", NA, "", " ", "11")
  )
  
  expect_equal(
    result,
    c(10, 11)
  )
})


test_that("normalise_indicator_ids keeps non-numeric characters", {
  
  result <- normalise_indicator_ids(
    c("10", "ABC", "12")
  )
  
  expect_equal(
    result,
    c("10", "ABC", "12")
  )
  
  expect_type(
    result,
    "character"
  )
})


test_that("normalise_indicator_ids rejects unsupported input types", {
  
  expect_error(
    normalise_indicator_ids(TRUE),
    "must be NULL.*numeric/character vector"
  )
})