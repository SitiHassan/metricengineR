#' Run a SQL script from a file
#'
#' Reads a SQL script from a file and executes it against an existing
#' database connection.
#'
#' @param conn An active database connection, typically created using
#'   `DBI::dbConnect()`.
#'
#' @param path A character scalar specifying the path to the SQL file
#'   to be executed.
#'
#' @return `NULL` invisibly when the SQL script runs successfully.
#'   A success message is printed to the console.
#'
#' @details
#' The SQL file is read line by line using `readLines()` and collapsed into
#' a single SQL string. The resulting SQL script is then executed using
#' `DBI::dbExecute()`.
#'
#' If the SQL script is executed successfully, a confirmation message is
#' printed containing the name of the SQL file.
#'
#' If an error occurs while reading or executing the SQL script, the error
#' is caught using `tryCatch()`. A descriptive error message is raised using
#' `stop()`, including the SQL filename and the original database or file
#' error message. Execution of the current pipeline is stopped.
#'
#' @section Dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `readLines()` - reads the contents of the SQL file.
#'   \item `basename()` - extracts the SQL filename from the supplied path.
#'   \item `DBI::dbExecute()` - executes the SQL script against the database.
#'   \item `tryCatch()` - handles errors encountered during execution.
#' }
#'
#' @export
run_sql_file <- function(conn, path) {
  
  tryCatch(
    {
      if(!file.exists(path)){
        stop(
          "SQL file does not exist: ", path, call. = FALSE
        )
      }
      
      sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      
      DBI::dbExecute(conn, sql_text)
      
      message("\u2705 SQL script successfully run: ", basename(path))
    },
    error = function(e){
      stop(
        "\u274C SQL script failed: ", basename(path), " - ", conditionMessage(e), call. = FALSE
      )
    }
  )
}

#' Extract indicators from a SQL table
#'
#' Extracts indicator data from a SQL database table using an existing
#' database connection. The function can return all indicators or filter
#' the data to a specified set of indicator IDs.
#'
#' @param conn An active database connection, typically created using
#'   `DBI::dbConnect()`.
#'
#' @param schema_name A character scalar specifying the database schema
#'   containing the table.
#'
#' @param table_name A character scalar specifying the name of the SQL table
#'   from which indicator data should be extracted.
#'
#' @param database_name An optional character scalar specifying the database
#'   containing the table. If `NULL`, the database associated with the
#'   existing connection is used. Defaults to `NULL`.
#'
#' @param indicator_ids An optional numeric or character vector containing
#'   valid indicator IDs to extract. Accepted values include:
#'
#'   \itemize{
#'     \item `NULL` to extract all indicators.
#'     \item `"All"` to extract all indicators.
#'     \item A numeric vector such as `c(1, 2, 3)`.
#'     \item A character vector such as `c("1", "2", "3")`.
#'   }
#'
#'   Comma-separated strings such as `"1, 2, 3"` are not accepted.
#'   `"All"` must be supplied on its own. Defaults to `NULL`.
#'
#' @return A data frame containing the records returned by the SQL query.
#'   On successful execution, messages are printed confirming that the
#'   extraction completed and reporting the total number of rows extracted.
#'
#' @details
#' The function first validates `indicator_ids` to ensure that the supplied
#' values are either numeric or character.
#'
#' A SQL table identifier is then created using `DBI::Id()`. When
#' `database_name` is supplied, the identifier contains the database, schema,
#' and table. When `database_name` is `NULL`, only the schema and table are
#' used.
#'
#' The table identifier is safely quoted using `DBI::dbQuoteIdentifier()`.
#'
#' If `indicator_ids` is `NULL` or equal to `"All"`, all rows from the
#' specified table are returned. Otherwise, each indicator ID is safely
#' quoted using `DBI::dbQuoteLiteral()` and added to a
#' `WHERE indicator_id IN (...)` clause.
#'
#' The resulting SQL query is executed using `DBI::dbGetQuery()`.
#'
#' Errors occurring while constructing or executing the SQL query are caught
#' using `tryCatch()`. If an error occurs, a descriptive error is raised using
#' `stop()` and execution of the current pipeline is stopped.
#'
#' @section Dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `DBI::Id()` - creates a database table identifier.
#'   \item `DBI::dbQuoteIdentifier()` - safely quotes the database table
#'     identifier.
#'   \item `DBI::dbQuoteLiteral()` - safely quotes indicator ID values.
#'   \item `DBI::dbGetQuery()` - executes the SQL query and returns the result.
#'   \item `tryCatch()` - handles errors during SQL query construction and
#'     execution.
#' }
#'
#' @export
get_indicators_from_sql <- function(conn,
                                    schema_name,
                                    table_name,
                                    database_name = NULL,
                                    indicator_ids = NULL){
  
      
      # Validate indicator IDs 
      valid_type <- is.character(indicator_ids) || is.numeric(indicator_ids) # must be either numeric or character
      
      if(!is.null(indicator_ids) && !valid_type){
        stop(
          "`indicator_ids` must be NULL, \"All\", or a numeric/character vector."
        )
      }
      
      if (is.character(indicator_ids) && any(grepl(",", indicator_ids))) { # don't accept this format: "1, 2, 3"
        stop(
          "`indicator_ids` must be supplied as a vector, for example ",
          "c(\"1\", \"2\", \"3\"), not as a single comma-separated string."
        )
      }
      
      if (length(indicator_ids) > 1 && any(tolower(as.character(indicator_ids)) == "all")) { # don't accept this format: c("All", "1")
        stop(
          "`All` must be supplied on its own."
        )
      }
    
  
      tryCatch(
        { 
          # Create table identifier
          if(is.null(database_name)){
            
            table_id <- DBI::Id(
              schema = schema_name,
              table = table_name
            )
          } else{
            
            table_id <- DBI::Id(
              catalog = database_name,
              schema = schema_name,
              table = table_name
            )
          }
          
          quoted_table <- DBI::dbQuoteIdentifier(
            conn, table_id
          )
          
          # Create a base query
          sql_query <- paste0("SELECT * FROM ", quoted_table)
          
          # Get ALL indicators if the following conditions are met
          get_all <- is.null(indicator_ids) || # If no IDs are provided, get all indicators by default
            (
              length(indicator_ids) == 1 &&
                is.character(indicator_ids) &&
                tolower(indicator_ids) == "all" # Or the supplied input is "All"
            )
          
          if (!get_all) {
            # Quote each literal safely to handle numbers/strings
            quoted_ids <- vapply(
              indicator_ids,
              function(x) {
                as.character(
                  DBI::dbQuoteLiteral(conn, x)
                )
              },
              character(1)
            )
            
            # Build the WHERE clause when indicator IDs are provided
            sql_query <- paste0(sql_query, " WHERE indicator_id IN (",
                                paste(quoted_ids, collapse = ", "),")")
          }
          
          # Run the query
          result <- DBI::dbGetQuery(conn, sql_query)
          
          message("\u2705 Indicators successfully extracted from SQL.")
          
          message("Total rows extracted: ",nrow(result))
          
          result
        },
        error = function(e) {
          stop(
            "\u274C Failed to extract indicators from SQL table ",
            table_name,
            " - ",
            conditionMessage(e),
            call. = FALSE
          )
        }
        )
}