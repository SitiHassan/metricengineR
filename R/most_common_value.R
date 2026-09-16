#' Find the Most Common Value
#'
#' Returns the most frequently occurring non-missing value in a vector.
#'
#' @param x A vector.
#'
#' @return The most common value as a character string, or `NA` if no
#'   non-missing values are available.
#'
#' @keywords internal
most_common_value <- function(x){
  
  # Helper: return the most common value 
  # This is used to learn the usual end month and day for a reporting cycle
  # E.g., if most Financial-year records end on 03-31, it infers that 31 March is the normal cycle end
  # If two values are tied for most common, which.max() returns the first one it encounters
  counts <- table(
    x,
    useNA = "no"
  )
  
  if(length(counts) == 0L){
    return(NA_character_)
  }
  
  names(counts)[
    which.max(counts)
  ]
}