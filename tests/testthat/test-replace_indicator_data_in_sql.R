test_that("selected indicator data are replaced correctly", {
  
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "Cluster_BBCS",
    Trusted_Connection = "True"
  )
  
  on.exit(
    DBI::dbDisconnect(conn)
  )
  
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE #metricengineR_test (
      indicator_id VARCHAR(50),
      indicator_value FLOAT
    )
    "
  )
  
  DBI::dbExecute(
    conn,
    "
    INSERT INTO #metricengineR_test
      (indicator_id, indicator_value)
    VALUES
      ('90', 10),
      ('91', 20),
      ('92', 30),
      ('93', 40)
    "
  )
  
  new_data <- data.frame(
    indicator_id = c("90", "91", "92"),
    indicator_value = c(100, 200, 300)
  )
  
  replace_indicator_data_in_sql(
    conn = conn,
    database = NULL,
    schema = NULL,
    table = "#metricengineR_test",
    data = new_data,
    indicator_ids = c("90", "91", "92")
  )
  
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM #metricengineR_test"
  )
  
  expect_equal(
    nrow(result),
    4
  )
  
  expect_equal(
    result$indicator_value[result$indicator_id == "90"],
    100
  )
  
  expect_equal(
    result$indicator_value[result$indicator_id == "91"],
    200
  )
  
  expect_equal(
    result$indicator_value[result$indicator_id == "92"],
    300
  )
  
  expect_equal(
    result$indicator_value[result$indicator_id == "93"],
    40
  )
})

test_that("All replaces all data in the temporary table", {
  
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "Cluster_BBCS",
    Trusted_Connection = "True"
  )
  
  on.exit(
    DBI::dbDisconnect(conn)
  )
  
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE #metricengineR_test (
      indicator_id VARCHAR(50),
      indicator_value FLOAT
    )
    "
  )
  
  DBI::dbExecute(
    conn,
    "
    INSERT INTO #metricengineR_test
      (indicator_id, indicator_value)
    VALUES
      ('90', 10),
      ('91', 20),
      ('92', 30),
      ('93', 40)
    "
  )
  
  new_data <- data.frame(
    indicator_id = c("100", "101"),
    indicator_value = c(500, 600)
  )
  
  replace_indicator_data_in_sql(
    conn = conn,
    database = NULL,
    schema = NULL,
    table = "#metricengineR_test",
    data = new_data,
    indicator_ids = "All"
  )
  
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM #metricengineR_test"
  )
  
  expect_equal(
    nrow(result),
    2
  )
  
  expect_setequal(
    result$indicator_id,
    c("100", "101")
  )
})

