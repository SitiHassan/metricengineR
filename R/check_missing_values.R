#' Check Missing Values
#'
#' Checks selected columns for missing values for non-precalculated indicators
#' with the specified status codes.
#'
#' @param df A data frame containing indicator data to check.
#' @param metadata A data frame containing indicator metadata, including
#'   `indicator_id`, `status_code`, and `precalculated`.
#' @param cols Optional character vector of columns to check. If `NULL`, all
#'   columns are checked except those listed in `ignore`.
#' @param ignore Character vector of columns to exclude from the check.
#'   Defaults to `numerator`, `denominator`, `lower_ci95`, and `upper_ci95`.
#' @param status_codes Integer vector of indicator status codes to include.
#'   Defaults to `c(1L, 2L)`, representing current and no-longer-refreshed
#'   indicators.
#' @param show_n Maximum number of summary rows to print. Defaults to `10L`.
#'
#' @return A data frame containing rows with missing values in the checked
#'   columns. Returns an empty data frame if no missing values are found.
#'
#' @details
#' Only indicators where `precalculated == "No"` and `status_code` matches one
#' of `status_codes` are included in the check.
#'
#' @export
check_missing_values <- function(
    df,
    metadata,
    cols = NULL,
    ignore = c(
      "numerator",
      "denominator",
      "lower_ci95",
      "upper_ci95"
    ),
    status_codes = c(1L, 2L),
    show_n = 10L
) {
  
  # Validate inputs 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(metadata)){
    stop(
      "`metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_metadata_cols <- c(
    "indicator_id",
    "status_code",
    "precalculated"
  )
  
  missing_metadata_cols <- setdiff(
    required_metadata_cols,
    names(metadata)
  )
  
  if(length(missing_metadata_cols) > 0){
    stop(
      paste0(
        "Missing required metadata columns: ",
        paste(missing_metadata_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if(!"indicator_id" %in% names(df)){
    stop(
      "Missing required column: indicator_id",
      call. = FALSE
    )
  }
  
  if(
    !is.numeric(show_n) ||
    length(show_n) != 1L ||
    is.na(show_n) ||
    show_n < 1 ||
    show_n %% 1 != 0
  ){
    stop(
      "`show_n` must be a whole number greater than or equal to 1.",
      call. = FALSE
    )
  }
  
  
  # Identify active, non-precalculated indicators 
  
  current_ids <- metadata |>
    dplyr::filter(
      .data$status_code %in% status_codes,
      .data$precalculated == "No"
    ) |>
    dplyr::distinct(
      .data$indicator_id
    ) |>
    dplyr::pull(
      .data$indicator_id
    )
  
  
  # Determine which columns should be checked 
  
  if(is.null(cols)){
    
    cols_to_check <- setdiff(
      names(df),
      ignore
    )
    
  } else {
    
    missing_requested_cols <- setdiff(
      cols,
      names(df)
    )
    
    if(length(missing_requested_cols) > 0){
      stop(
        paste0(
          "Columns not found in `df`: ",
          paste(missing_requested_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    cols_to_check <- cols
  }
  
  
  # Find rows containing missing values 
  
  missing_rows <- df |>
    dplyr::filter(
      .data$indicator_id %in% current_ids
    ) |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(cols_to_check),
        ~ is.na(.x)
      )
    )
  
  
  # Report results 
  
  if(nrow(missing_rows) == 0L){
    
    message(
      "\u2705 PASS: No rows with missing values in the checked columns."
    )
    
  } else {
    
    missing_summary <- missing_rows |>
      dplyr::select(
        .data$indicator_id,
        dplyr::all_of(
          setdiff(
            cols_to_check,
            "indicator_id"
          )
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          -dplyr::all_of("indicator_id"),
          ~ is.na(.x)
        )
      ) |>
      tidyr::pivot_longer(
        cols = -dplyr::all_of("indicator_id"),
        names_to = "missing_column",
        values_to = "is_missing"
      ) |>
      dplyr::filter(
        .data$is_missing
      ) |>
      dplyr::count(
        .data$indicator_id,
        .data$missing_column,
        name = "missing_rows"
      ) |>
      dplyr::arrange(
        .data$indicator_id,
        .data$missing_column
      )
    
    message(
      "\u26A0\uFE0F WARNING: Found ",
      nrow(missing_rows),
      " row(s) with missing values in the checked columns."
    )
    
    print(
      utils::head(
        missing_summary,
        show_n
      )
    )
  }
  
  
  # Return rows containing missing values 
  
  missing_rows
}