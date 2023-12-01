#' Query keywords from a database table.
#' @title Query Keywords
#' @description query keywords from database table
#' @param tbl the database table
#' @param kwds key words
#' @param column the column where searching the key words
#' @param ignore_case whether the case will be ignored when searching for the key word. Default is TRUE
#' @param match_all whether the selected value shoule match all the key words. Default is FALSE
#' @return a tibble with the selected values
#' @importFrom dplyr filter sql
#' @export
query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )

  dplyr::filter(tbl, dplyr::sql(query))
}
