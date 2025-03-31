#' Compute Relative Day Variables
#'
#' Computes the relative day (`--DY`) from a reference date for given date or datetime columns.
#'
#' @param dataset A data frame containing date or datetime variables.
#' @param reference_date A column name (unquoted) representing the reference date.
#' @param source_vars A character vector of column names from which relative days will be derived.
#'
#' @import dplyr
#' @import rlang
#' @import lubridate
#' @return A data frame with new `--DY` columns corresponding to `source_vars`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' data <- tibble(
#'   TRTSDT = as.Date("2024-03-01"),
#'   ASTDT = as.Date("2024-03-05"),
#'   AENDT = as.Date("2024-03-10")
#' )
#'
#' relative_day(data, reference_date = TRTSDT, source_vars = c("ASTDT", "AENDT"))
relative_day <- function(dataset, reference_date, source_vars) {
  # Ensure reference_date is a symbol using enquo
  reference_date <- rlang::enquo(reference_date)

  # Convert the source_vars into symbols using syms
  source_vars <- rlang::syms(source_vars)

  # Check if the specified source variables exist in the dataset
  missing_vars <- setdiff(source_vars, names(dataset))
  if (length(missing_vars) > 0) {
    stop("Missing variables in dataset: ", paste(missing_vars, collapse = ", "))
  }

  # Use mutate to calculate the relative days for ASTDY and AENDY
  dataset <- dataset %>%
    dplyr::mutate(
      ASTDY = as.integer(as.Date(!!source_vars[[1]]) - as.Date(!!reference_date) + 1),
      AENDY = as.integer(as.Date(!!source_vars[[2]]) - as.Date(!!reference_date) + 1)
    )

  return(dataset)
}

