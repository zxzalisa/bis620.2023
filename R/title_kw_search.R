#' @title Query keywords in brief title from a data set
#' @description query keywords in brief title from database table
#' @param studies the database table
#' @param kw key words
#' @return a tibble with the selected values
#' @export
title_kw_search <- function(studies, kw) {
  query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    collect()
}
