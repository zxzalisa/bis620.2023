#' Condition Histogram
#'
#' Create a histogram of a specific number of conditions
#'
#' @param d the table used to plot
#' @param n_conditions the number of conditions that show in the histogram
#' @return a histogram of the conditions
#' @importFrom utils head
#' @importFrom dplyr arrange desc
#' @importFrom tidyr unnest
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @export
create_condition_histogram= function(d,n_conditions) {

  d$phase[is.na(d$phase)] <- "NA"

  data <- left_join(as.data.frame(d),as.data.frame(conditions),by="nct_id")

  data_con <- data |>
    select(nct_id,downcase_name) |>
    unnest(downcase_name) |>
    group_by(downcase_name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    head(n_conditions)

  ggplot(data_con, aes(x = downcase_name, y = n)) +
    geom_col() +
    scale_y_log10() +
    theme_bw()+
    xlab("Conditions") +
    ylab("Count")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
