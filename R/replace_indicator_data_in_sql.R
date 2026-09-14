#' Replace indicator data in a SQL table
#'
#' Deletes existing records from a SQL table and appends new metric data using
#' an existing database connection. The function can either replace all rows
#' in the table or replace only rows associated with selected indicator IDs.
#'
#' @param conn An active database connection, typically created using
#'   `DBI::dbConnect()`.
#'
#' @param database_name A character scalar specifying the database containing the
#'   target table.
#'
#' @param schema_name A character scalar specifying the schema containing the target
#'   table.
#'
#' @param table_name A character scalar specifying the name of the SQL table into
#'   which the data should be loaded.
#'
#' @param data A data frame containing the records to append to the SQL table
#'   after the existing records have been deleted.
#'
#' @param indicator_ids A vector of numeric/character indicator IDs identifying which existing
#'   records should be deleted before the new data are appended.
#'
#'   When `"All"` is supplied, all existing rows in the table are deleted
#'   before the new data are appended. Defaults to `"All"`.
#'
#' @param id_column A character scalar specifying the name of the column used
#'   to identify indicators in the target SQL table. Defaults to
#'   `"indicator_id"`.
#'
#' @return `NULL` invisibly when the operation completes successfully.
#'   A message is printed indicating whether all rows or only selected
#'   indicator rows were replaced.
#'
#' @details
#' The supplied `indicator_ids` are first processed using
#' `normalise_indicator_ids()`.
#'
#' The target table and indicator ID column are represented using
#' `DBI::Id()` and safely quoted using `DBI::dbQuoteIdentifier()`.
#'
#' If specific indicator IDs are supplied, the function constructs a
#' `DELETE` statement containing a `WHERE` clause for those IDs. Indicator
#' values are safely quoted using `DBI::dbQuoteLiteral()`.
#'
#' If `"All"` is supplied and `normalise_indicator_ids()` returns `NULL`,
#' no `WHERE` clause is added and all existing rows in the target table are
#' deleted.
#'
#' The deletion and subsequent append operation are performed inside a single
#' database transaction using `DBI::dbWithTransaction()`. This ensures that if
#' the insert fails after the deletion has started, the transaction is rolled
#' back rather than leaving the target table partially updated.
#'
#' New data are appended using `DBI::dbWriteTable(..., append = TRUE)`.
#'
#' Errors occurring during table identification, deletion, or insertion are
#' caught using `tryCatch()`. If an error occurs, a descriptive error is
#' raised using `stop()` and execution of the current pipeline is stopped.
#'
#' @section Internal dependencies:
#' This function depends on:
#'
#' \itemize{
#'   \item `normalise_indicator_ids()` - standardises the supplied indicator
#'     ID input
#' }
#'
#' @section External dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `DBI::Id()` - creates the database table identifier.
#'   \item `DBI::dbQuoteIdentifier()` - safely quotes table and column
#'     identifiers.
#'   \item `DBI::dbQuoteLiteral()` - safely quotes indicator ID values.
#'   \item `DBI::dbWithTransaction()` - performs deletion and insertion within
#'     a single database transaction.
#'   \item `DBI::dbExecute()` - executes the SQL `DELETE` statement.
#'   \item `DBI::dbWriteTable()` - appends the supplied data to the target SQL
#'     table.
#'   \item `tryCatch()` - handles errors during the database operation.
#' }
#'
#' @export
replace_indicator_data_in_sql <- function(conn, 
                                          database_name, 
                                          schema_name, 
                                          table_name, 
                                          data, 
                                          indicator_ids = "All",
                                          id_column = "indicator_id") {
  
  # normalise indicator IDs
  ids <- normalise_indicator_ids(indicator_ids)
  
  tryCatch(
    {
      # Create and safely quote table/column identifiers
      if (startsWith(table_name, "#")) {
        
        tbl_id <- table_name
        
      } else{
        # Create and safely quote table/column identifiers
        tbl_id <- DBI::Id(
          catalog = database_name,
          schema = schema_name,
          table = table_name
        )
      }
      
      tbl_sql <- DBI::dbQuoteIdentifier(conn, tbl_id)
      col_sql <- DBI::dbQuoteIdentifier(conn, id_column)
      
      # Build DELETE query
      sql_query <- paste0("DELETE FROM ", tbl_sql)
      
      if (!is.null(ids) && length(ids) > 0) {
        
        quoted_vals <- vapply(
          ids,
          function(x) {
            as.character(
              DBI::dbQuoteLiteral(conn, x)
            )
          },
          character(1)
        )
        
        # Build WHERE clause when IDs are provided
        sql_query <- paste0(
          sql_query,
          " WHERE ",
          col_sql,
          " IN (",
          paste(quoted_vals, collapse = ", "),
          ")"
        )
      }
      
      # Delete and insert within a single transaction
      DBI::dbWithTransaction( # Protects the database from ending up half-updated
        conn,
        {
          DBI::dbExecute(
            conn,
            sql_query
          )
          
          DBI::dbWriteTable(
            conn,
            name = tbl_id,
            value = data,
            append = TRUE
          )
        }
      )
      
      # Success messages
      if (is.null(ids) || length(ids) == 0) { # All rows are deleted when "All" is supplied (normalised "All" will be NULL)
        
        message(
          "\u2705 All rows deleted and new data appended to ",
          tbl_sql,
          "."
        )
        
      } else {
        
        message(
          "\u2705 Deleted and replaced data for selected indicator_ids in ",
          tbl_sql,
          "."
        )
      }
    },
    error = function(e) {
      stop(
        "\u274C Failed to insert data into SQL table ",
        table_name,
        " - ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}