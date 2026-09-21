#' Get Indicators Requiring Pooling
#'
#' Identifies indicators that require pooled data.
#'
#' @param metadata A data frame containing indicator metadata.
#' @param indicator_col Name of the indicator ID column.
#'   Defaults to `"indicator_id"`.
#' @param pooling_col Name of the logical column indicating whether an
#'   indicator requires pooling. Defaults to `"requires_pooling"`.
#'
#' @return A vector of indicator IDs where `requires_pooling` is `TRUE`.
#'
#' @details
#' The pooling column should contain logical `TRUE` or `FALSE` values.
#' Indicators marked as `TRUE` can then be used to create pooled data,
#' such as 3-year and 5-year pooled periods.
#'
#' @export
get_indicators_requiring_pooling <- function(
    metadata,
    indicator_col = "indicator_id",
    pooling_col = "requires_pooling"
) {
  
  # Validate input
  
  if(!is.data.frame(metadata)){
    stop(
      "`metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    indicator_col,
    pooling_col
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(metadata)
  )
  
  if(length(missing_cols) > 0L){
    stop(
      paste0(
        "Columns not found in `metadata`: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if(!is.logical(metadata[[pooling_col]])){
    stop(
      paste0(
        "`",
        pooling_col,
        "` must contain TRUE/FALSE values."
      ),
      call. = FALSE
    )
  }
  
  
  # Identify indicators requiring pooling
  
  pooled_ids <- metadata |>
    dplyr::filter(
      .data[[pooling_col]]
    ) |>
    dplyr::distinct(
      .data[[indicator_col]]
    ) |>
    dplyr::pull(
      .data[[indicator_col]]
    )
  
  
  pooled_ids
}