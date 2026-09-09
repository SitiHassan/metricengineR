test_that("get_indicators_from_sql extracts specified indicators", {
  
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "Cluster_BBCS",
    Trusted_Connection = "True"
  )
  
  on.exit(DBI::dbDisconnect(conn))
  
  ids <- c("90", "91", "92")
  table_name <- "Oversight_Framework_Fact_Final_Input_Data"
  schema_name <- "BBCS"
  
  result <- get_indicators_from_sql(
    conn = conn,
    schema_name = schema_name,
    table_name = table_name,
    indicator_ids = ids
  )
  
  expect_s3_class(result, "data.frame")
  
  expect_gt(
    nrow(result),
    0
  )
  
  expect_true(
    all(as.character(result$indicator_id) %in% ids)
  )
  
  expect_setequal(
    unique(as.character(result$indicator_id)),
    ids
  )
})

test_that("get_indicators_from_sql rejects comma-separated indicator IDs", {
  
  expect_error(
    get_indicators_from_sql(
      conn = NULL,
      schema_name = "BBCS",
      table_name = "Oversight_Framework_Fact_Final_Input_Data",
      indicator_ids = "90, 91, 92"
    ),
    "must be supplied as a vector"
  )
})

test_that("get_indicators_from_sql rejects All combined with other IDs", {
  
  expect_error(
    get_indicators_from_sql(
      conn = NULL,
      schema_name = "BBCS",
      table_name = "Oversight_Framework_Fact_Final_Input_Data",
      indicator_ids = c("All", "90")
    ),
    "All.*must be supplied on its own"
  )
})

test_that("get_indicators_from_sql rejects invalid indicator ID types", {
  
  expect_error(
    get_indicators_from_sql(
      conn = NULL,
      schema_name = "BBCS",
      table_name = "Oversight_Framework_Fact_Final_Input_Data",
      indicator_ids = TRUE
    ),
    "must be NULL.*numeric/character vector"
  )
})

test_that("get_indicators_from_sql extracts all indicators when All is supplied", {
  
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "Cluster_BBCS",
    Trusted_Connection = "True"
  )
  
  on.exit(DBI::dbDisconnect(conn))
  
  result <- get_indicators_from_sql(
    conn = conn,
    schema_name = "BBCS",
    table_name = "Oversight_Framework_Fact_Final_Input_Data",
    indicator_ids = "All"
  )
  
  expect_s3_class(result, "data.frame")
  
  expect_gt(
    nrow(result),
    0
  )
  
  expect_gt(
    length(unique(result$indicator_id)),
    3
  )
})