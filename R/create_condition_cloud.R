#' @title Conditions Word-cloud
#' @description create a word-coud plot of conditions
#' @param d the table used to plot
#' @param freq Minimum Frequency of Conditions
#' @param max Maximum Number of Conditions
#' @return a histogram of the interventions
create_condition_cloud <- function(d,freq=10,max=25){
  d$phase[is.na(d$phase)] <- "NA"
  data <- left_join(as.data.frame(d),as.data.frame(conditions),by = "nct_id")

  wordcloud_rep <- repeatable(wordcloud)
  data <- data |>
    select(downcase_name)|>
    as.data.frame()|>
    getTermMatrix()

  wordcloud_rep(names(data), data, scale=c(4,0.5),
                min.freq = freq, max.words=max,
                colors=brewer.pal(8, "Dark2"))
}
