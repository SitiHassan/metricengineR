# Helper function to create test SII data
create_test_sii_group <- function(
    indicator_id = 101L,
    n_deprivation_groups = 5L,
    numerators = NULL,
    denominators = NULL,
    imd_codes = NULL,
    aggregation_id = 151L,
    age_group_code = 18L,
    sex_code = 1L,
    ethnicity_code = 1L,
    value_type_code = 14L,
    combination_id = 1L){
  
  if(is.null(imd_codes)){
    imd_codes <- seq_len(n_deprivation_groups)
  }
  
  n_rows <- length(imd_codes)
  
  if(is.null(denominators)){
    denominators <- rep(1000, n_rows)
  }
  
  if(is.null(numerators)){
    deprivation_position <- 
      1 - (
        (seq_len(n_rows) - 0.5) /
          n_rows
      )
    
    proportions <- 
      0.10 + 0.20 * deprivation_position
    
    numerators <- proportions * denominators
  }
  
  data.frame(
    indicator_id = rep(as.integer(indicator_id), n_rows),
    start_date = rep(as.Date("2025-01-01"), n_rows),
    end_date = rep(as.Date("2025-12-31"), n_rows),
    numerator = numerators,
    denominator = denominators,
    indicator_value = rep(NA_real_, n_rows),
    lower_ci95 = rep(NA_real_, n_rows),
    upper_ci95 = rep(NA_real_, n_rows),
    imd_code = as.integer(imd_codes),
    aggregation_id = rep(as.integer(aggregation_id), n_rows),
    age_group_code = rep(as.integer(age_group_code), n_rows),
    sex_code = rep(as.integer(sex_code), n_rows),
    ethnicity_code = rep(as.integer(ethnicity_code), n_rows),
    creation_date = rep(as.Date("2026-01-01"), n_rows),
    value_type_code = rep(as.integer(value_type_code), n_rows),
    source_code = rep(1L, n_rows),
    time_period_type = rep("1 year", n_rows),
    combination_id = rep(as.integer(combination_id), n_rows)
  )
}


# Test 1: Basic SII calculation
# Proportions are 0.28, 0.24, 0.20, 0.16 and 0.12.
# These follow a linear gradient with a slope of 0.20.
# Expected absolute SII = 20 percentage points.
testthat::test_that("calculate_sii calculates the expected SII", {
  
  df <- create_test_sii_group()
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    nrow(result),
    1
  )
  
  testthat::expect_equal(
    result$value,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_signed_percentage_points,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_absolute_percentage_points,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_direction,
    "Higher in most deprived"
  )
  
  testthat::expect_equal(
    result$numerator,
    1000
  )
  
  testthat::expect_equal(
    result$denominator,
    5000
  )
  
  testthat::expect_equal(
    result$imd_code,
    999L
  )
  
  testthat::expect_true(
    is.na(result$lowercl)
  )
  
  testthat::expect_true(
    is.na(result$uppercl)
  )
})


# Test 2: Negative SII direction is returned as an absolute value
testthat::test_that("calculate_sii returns the absolute SII", {
  
  # Default value_output = "absolute"
  df <- create_test_sii_group(
    numerators = c(120, 160, 200, 240, 280)
  )
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    result$value,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_signed_percentage_points,
    -20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_absolute_percentage_points,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_direction,
    "Higher in least deprived"
  )
})

# Test 3: Negative SII direction is returned as a negative value when value_output is specified
testthat::test_that("calculate_sii returns signed SII when requested", {
  
  df <- create_test_sii_group(
    numerators = c(120, 160, 200, 240, 280)
  )
  
  result <- calculate_sii(
    df,
    value_output = "signed")
  
  testthat::expect_equal(
    result$value,
    -20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_signed_percentage_points,
    -20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_absolute_percentage_points,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_direction,
    "Higher in least deprived"
  )
})

# Test 4: Multiple SII groups are calculated separately
testthat::test_that("calculate_sii calculates separate SII groups", {
  
  group_1 <- create_test_sii_group(
    indicator_id = 101L,
    sex_code = 1L,
    combination_id = 1L,
    numerators = c(280, 240, 200, 160, 120)
  )
  
  group_2 <- create_test_sii_group(
    indicator_id = 102L,
    sex_code = 2L,
    ethnicity_code = 21L,
    combination_id = 2L,
    numerators = c(190, 170, 150, 130, 110)
  )
  
  df <- dplyr::bind_rows(
    group_1,
    group_2
  )
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_equal(
    result$value[result$indicator_id == 101L],
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$value[result$indicator_id == 102L],
    10,
    tolerance = 1e-8
  )
})


# Test 5: Only SII value type rows are calculated
testthat::test_that("calculate_sii filters to value type code 14", {
  
  sii_data <- create_test_sii_group(
    indicator_id = 101L
  )
  
  non_sii_data <- create_test_sii_group(
    indicator_id = 102L,
    value_type_code = 2L
  )
  
  df <- dplyr::bind_rows(
    sii_data,
    non_sii_data
  )
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    nrow(result),
    1
  )
  
  testthat::expect_equal(
    result$indicator_id,
    101L
  )
})


# Test 6: Custom column names work
testthat::test_that("calculate_sii works with custom column names", {
  
  df <- create_test_sii_group() |>
    dplyr::rename(
      type_code = value_type_code,
      deprivation = imd_code,
      events = numerator,
      population = denominator
    )
  
  result <- calculate_sii(
    df,
    value_type_code_col = "type_code",
    deprivation_group_col = "deprivation",
    numerator_col = "events",
    denominator_col = "population"
  )
  
  testthat::expect_equal(
    result$value,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$numerator,
    1000
  )
  
  testthat::expect_equal(
    result$denominator,
    5000
  )
})


# Test 7: Quintile values stored as character can be converted
testthat::test_that("calculate_sii accepts quintile codes stored as character", {
  
  df <- create_test_sii_group()
  
  df$imd_code <- as.character(
    df$imd_code
  )
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    result$value,
    20,
    tolerance = 1e-8
  )
})


# Test 8: Confidence intervals are not currently implemented
testthat::test_that("calculate_sii stops when confidence intervals are required", {
  
  df <- create_test_sii_group()
  
  testthat::expect_error(
    calculate_sii(
      df,
      confidence_intervals_required = TRUE
    ),
    "Confidence intervals for SII have not yet been implemented"
  )
})


# Test 9: Non-data-frame inputs are rejected
testthat::test_that("calculate_sii rejects non-data-frame input", {
  
  testthat::expect_error(
    calculate_sii(c(1, 2, 3)),
    "`df` must be a data frame"
  )
})


# Test 10: Missing required columns are reported
testthat::test_that("calculate_sii reports missing required columns", {
  
  df <- create_test_sii_group() |>
    dplyr::select(
      -denominator
    )
  
  testthat::expect_error(
    calculate_sii(df),
    "Missing required columns: denominator"
  )
})


# Test 11: confidence_intervals_required must be TRUE or FALSE
testthat::test_that("calculate_sii validates confidence_intervals_required", {
  
  df <- create_test_sii_group()
  
  testthat::expect_error(
    calculate_sii(
      df,
      confidence_intervals_required = "TRUE"
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      confidence_intervals_required = c(TRUE, FALSE)
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      confidence_intervals_required = NA
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
})


# Test 12: Missing numerator or denominator values are rejected
testthat::test_that("calculate_sii rejects missing numerator and denominator values", {
  
  df <- create_test_sii_group()
  df$numerator[1] <- NA
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator and denominator cannot contain missing values"
  )
  
  df <- create_test_sii_group()
  df$denominator[1] <- NA
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator and denominator cannot contain missing values"
  )
})


# Test 13: Numerator and denominator must be numeric
testthat::test_that("calculate_sii rejects non-numeric numerator or denominator", {
  
  df <- create_test_sii_group()
  df$numerator <- as.character(df$numerator)
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator and denominator must be numeric"
  )
  
  df <- create_test_sii_group()
  df$denominator <- as.character(df$denominator)
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator and denominator must be numeric"
  )
})


# Test 14: Denominator must be greater than zero
testthat::test_that("calculate_sii rejects zero or negative denominators", {
  
  df <- create_test_sii_group()
  df$denominator[1] <- 0
  
  testthat::expect_error(
    calculate_sii(df),
    "Denominator must be greater than zero"
  )
  
  df <- create_test_sii_group()
  df$denominator[1] <- -100
  
  testthat::expect_error(
    calculate_sii(df),
    "Denominator must be greater than zero"
  )
})


# Test 15: Numerator cannot be negative
testthat::test_that("calculate_sii rejects negative numerators", {
  
  df <- create_test_sii_group()
  df$numerator[1] <- -1
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator cannot be negative"
  )
})


# Test 16: Numerator cannot exceed denominator
testthat::test_that("calculate_sii rejects numerator greater than denominator", {
  
  df <- create_test_sii_group()
  
  df$numerator[1] <- 1001
  
  testthat::expect_error(
    calculate_sii(df),
    "Numerator cannot be greater than denominator"
  )
})


# Test 17: Each SII group must contain quintiles 1 to 5
testthat::test_that("calculate_sii requires exactly one row for each quintile", {
  
  df <- create_test_sii_group(
    imd_codes = c(1, 2, 3, 4, 4)
  )
  
  testthat::expect_error(
    calculate_sii(df),
    "Each SII calculation must contain exactly one row for deprivation groups 1 to 5"
  )
})


# Test 18: Missing quintile is rejected
testthat::test_that("calculate_sii rejects incomplete quintile groups", {
  
  df <- create_test_sii_group()
  
  df <- df[
    df$imd_code != 5L,
  ]
  
  testthat::expect_error(
    calculate_sii(df),
    "Each SII calculation must contain exactly one row for deprivation groups 1 to 5"
  )
})


# Test 19: Proportions equal to zero or one are rejected
# These produce zero binomial variance and therefore undefined weights.
testthat::test_that("calculate_sii rejects zero variance quintiles", {
  
  df <- create_test_sii_group()
  df$numerator[1] <- 0
  
  testthat::expect_error(
    calculate_sii(df),
    "SII variance must be positive and finite"
  )
  
  df <- create_test_sii_group()
  df$numerator[1] <- df$denominator[1]
  
  testthat::expect_error(
    calculate_sii(df),
    "SII variance must be positive and finite"
  )
})


# Test 20: No SII rows returns an empty result
testthat::test_that("calculate_sii handles data with no SII rows", {
  
  df <- create_test_sii_group()
  
  df$value_type_code <- 2L
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    nrow(result),
    0
  )
  
  testthat::expect_true(
    all(
      c(
        "value",
        "lowercl",
        "uppercl"
      ) %in% names(result)
    )
  )
})

# Test 21: Function calculates SII using deprivation deciles
testthat::test_that("calculate_sii calculates SII using deprivation deciles", {
  
  df <- create_test_sii_group(
    n_deprivation_groups = 10L
  )
  
  result <- calculate_sii(
    df,
    n_deprivation_groups = 10L
  )
  
  testthat::expect_equal(
    nrow(result),
    1
  )
  
  testthat::expect_equal(
    result$value,
    20,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$numerator,
    2000,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$denominator,
    10000
  )
})

# Test 22: Function rejects when input deprivation groups do not match requested groups
testthat::test_that("calculate_sii requires input groups to match requested groups", {
  
  df <- create_test_sii_group(
    n_deprivation_groups = 10L
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      n_deprivation_groups = 5L
    ),
    "Each SII calculation must contain exactly one row for deprivation groups 1 to 5"
  )
})

# Test 23: Function rejects invalid number of deprivation groups
testthat::test_that("calculate_sii validates n_deprivation_groups", {
  
  df <- create_test_sii_group()
  
  testthat::expect_error(
    calculate_sii(
      df,
      n_deprivation_groups = 1
    ),
    "`n_deprivation_groups` must be a whole number greater than 1"
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      n_deprivation_groups = 5.5
    ),
    "`n_deprivation_groups` must be a whole number greater than 1"
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      n_deprivation_groups = NA
    ),
    "`n_deprivation_groups` must be a whole number greater than 1"
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      n_deprivation_groups = "5"
    ),
    "`n_deprivation_groups` must be a whole number greater than 1"
  )
})

# Test 24: Function rejects invalid value_output parameter
testthat::test_that("calculate_sii validates value_output", {
  
  df <- create_test_sii_group()
  
  testthat::expect_error(
    calculate_sii(
      df,
      value_output = "something"
    ),
    "`value_output` must be either \"absolute\" or \"signed\""
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      value_output = c("absolute", "signed")
    ),
    "`value_output` must be either \"absolute\" or \"signed\""
  )
  
  testthat::expect_error(
    calculate_sii(
      df,
      value_output = NA_character_
    ),
    "`value_output` must be either \"absolute\" or \"signed\""
  )
})

# Test 25: Function identifies no deprivation gradient
testthat::test_that("calculate_sii identifies no deprivation gradient", {
  
  df <- create_test_sii_group(
    numerators = rep(200, 5)
  )
  
  result <- calculate_sii(df)
  
  testthat::expect_equal(
    result$value,
    0,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$sii_direction,
    "No deprivation gradient"
  )
})