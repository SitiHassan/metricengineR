#' Create Inequality Combination ID
#'
#' Creates a `combination_id` that identifies whether each row contains an
#' IMD split, an ethnicity split, both, or neither.
#'
#' This can be used downstream to distinguish overall results from inequality
#' breakdowns when filtering, reporting, or visualising metric-engine output.
#'
#' The combination IDs are:
#'
#' \itemize{
#'   \item `1` - IMD and ethnicity splits are both present
#'   \item `2` - IMD split only
#'   \item `3` - Ethnicity split only
#'   \item `4` - Neither split is present; the row represents an overall result
#' }
#'
#' The value defined by `overall_code` is treated as representing no split.
#' By default, this is `999`.
#'
#' @param df A data frame containing IMD and ethnicity code columns.
#' @param imd_code_col Name of the column containing IMD codes.
#'   Defaults to `"imd_code"`.
#' @param ethnicity_code_col Name of the column containing ethnicity codes.
#'   Defaults to `"ethnicity_code"`.
#' @param overall_code Code used to represent an overall result with no
#'   inequality split. Defaults to `999L`.
#'
#' @return The original data frame with an additional integer
#'   `combination_id` column.
#'
#' @details
#' Missing or unexpected IMD or ethnicity values are assigned
#' `NA` for `combination_id`.
#'
#' @export
create_inequality_combination_id <- function(
    df,
    imd_code_col = "imd_code",
    ethnicity_code_col = "ethnicity_code",
    overall_code = 999L
) {
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    imd_code_col,
    ethnicity_code_col
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0){
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  result <- df |>
    dplyr::mutate(
      combination_id = dplyr::case_when(
        
        .data[[imd_code_col]] != overall_code &
          .data[[ethnicity_code_col]] != overall_code ~ 1L, # Both splits present
        
        .data[[imd_code_col]] != overall_code &
          .data[[ethnicity_code_col]] == overall_code ~ 2L, # Only IMD split present
        
        .data[[imd_code_col]] == overall_code &
          .data[[ethnicity_code_col]] != overall_code ~ 3L, # Only Ethnicity split present
        
        .data[[imd_code_col]] == overall_code &
          .data[[ethnicity_code_col]] == overall_code ~ 4L, # Neither (Overall)
        
        TRUE ~ NA_integer_
      )
    )
  
  result
}