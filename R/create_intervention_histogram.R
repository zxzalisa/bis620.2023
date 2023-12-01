#' Intervention Histogram
#'
#' Create a histogram of interventions
#'
#' @param d the table used to plot
#' @return a histogram of the interventions
#' @importFrom utils head
#' @importFrom dplyr left_join select filter group_by summarize
#' @importFrom tidyr unnest
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @export
create_interventions_histogram <- function(d) {

  d <- d|> as.data.frame()

  d$phase[is.na(d$phase)] <- "NA"

  data <- left_join(as.data.frame(d),as.data.frame(interventions),by="nct_id")

  data <- data |>
    select(nct_id,intervention_type) |>
    unnest(intervention_type) |>
    group_by(intervention_type) |>
    summarize(n = n())

  ggplot(data, aes(x = intervention_type, y = n)) +
    geom_col() +
    scale_y_log10() +
    theme_bw()+
    xlab("Interventions") +
    ylab("Count")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
