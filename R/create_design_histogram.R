#' Design Histogram
#'
#' Create a histogram of the intervention study models.
#'
#' @param x the database table.
#' @param way the filtering choices of allocation ways
#' @return A histogram for the strategy for assigning interventions
#' to participants.
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @export
create_design_histogram <- function(x, way) {

  designs_data <- select_colomns(x, designs, intervention_model)
  colnames(designs_data)[2] <- "design"
  x <- left_join(x, designs_data, by = "nct_id")

  alct_data <- select_colomns(x, designs, allocation)
  colnames(alct_data)[2] <- "allocation"
  x <- left_join(x, alct_data, by = "nct_id")

  if (!is.null(way)) {
    x <- x |>
      filter(allocation %in% !!way)
  }

  x$design[is.na(x$design)] <- "NA"
  x$allocation[is.na(x$allocation)] <- "NA"

  x <- x |>
    select(design) |>
    group_by(design) |>
    summarize(n = n())

  x$design <- as.character(trimws(x$design))

  ggplot(x, aes(x = design, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Intervention Study Model") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
