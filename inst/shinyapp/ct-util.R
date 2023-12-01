check_environment <- function() {
  local_db_path <- "~/bis620.2023/inst/shinyapp/ctrialsgovdb/ctrialsgov.duckdb"

  if (file.exists(local_db_path)) {
    # Local file exists, proceed to use it
    con <- dbConnect(duckdb(
      file.path(local_db_path),
      read_only = TRUE
    ))
    # Perform operations with local DuckDB connection
  } else {

    # Local file does not exist, install package from GitHub
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }

    devtools::install_github("presagia-analytics/ctrialsgov")

    # Load the package after ensuring it's installed
    library(duckdb)
    con <- dbConnect(duckdb(file.path("ctrialsgovdb/ctrialsgov.duckdb"),
                            read_only = TRUE))
  }

  # Return any necessary objects or connections
  # For example, return the DuckDB connection if needed elsewhere
  return(con)
}

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}

#' @title Query Keywords
#' @description query keywords from database table
#' @param tbl the database table
#' @param kwds key words
#' @param column the column where searching the key words
#' @param ignore_case whether the case will be ignored
#' when searching for the key word. Default is TRUE
#' @param match_all whether the selected value should
#'  match all the key words. Default is FALSE
#' @return a tibble with the selected values
query_kwds <- function(tbl, kwds, column,
                       ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else {
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )

  dplyr::filter(tbl, dplyr::sql(query))
}

#' @title Phase Histogram
#' @description create a histogram of the phase
#' @param d the table used to plot
#' @return a histogram of the phase
create_phase_histogram_plot <- function(d) {
  d$phase[is.na(d$phase)] <- "NA"

  d <- d |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  #allphase is the character contains the name of all types of phases
  allphase <- studies |> select(phase) |> as_tibble() |> unique()
  allphase <- as.character(allphase$phase)
  allphase[is.na(allphase)] <- "NA"

  #allphase<-as.character("NA","Phase1")

  dnew <- d
  #we reorder the rows in d, and add n=0 for the phases do not occur in d.
  for(i in 1:length(allphase)){
    if(sum(d$phase == as.character(allphase)[i]) > 0 ){
      dnew[i,] <- d[d$phase==as.character(allphase)[i] , ]
    }else{
      dnew[i,] <- d[as.character(allphase)[i] , 0]
    }
  }

  ggplot(dnew, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}


#' @title Condition Histogram
#' @description create a histogram of a specific number of conditions
#' @param d the table used to plot
#' @param n_conditions the number of conditions that show in the histogram
#' @return a histogram of the conditions
create_condition_histogram <- function(d,n_conditions) {

  d$phase[is.na(d$phase)] <- "NA"

  data <- left_join(as.data.frame(d), as.data.frame(conditions), by="nct_id")

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

#' @title Word Frequency
#' @description  Get the frequency of words from the text.
#' @param x the text.
#' @return Frequency of words with descending order.
getTermMatrix <- memoise(function(x) {
  x = as.character(x)
  if (is.character(x)){
    myCorpus <- Corpus(VectorSource(x))
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    myCorpus <- tm_map(myCorpus, removePunctuation)
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    myDTM <- TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m <- as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  }
})

#' @title Conditions Word-cloud
#' @description create a word-coud plot of conditions
#' @param d the table used to plot
#' @param freq Minimum Frequency of Conditions
#' @param max Maximum Number of Conditions
#' @return a histogram of the interventions
create_condition_cloud <- function(d,freq=10,max=25){
  d$phase[is.na(d$phase)] <- "NA"
  data <- left_join(as.data.frame(d),as.data.frame(conditions),by="nct_id")

  wordcloud_rep <- repeatable(wordcloud)
  data <- data |>
    select(downcase_name)|>
    as.data.frame()|>
    getTermMatrix()

  wordcloud_rep(names(data), data, scale <- c(4,0.5),
                min.freq <- freq, max.words=max,
                colors <- brewer.pal(8, "Dark2"))
}

#' @title Create Intervention Histogram Plot
#' @description create a histogram of interventions
#' @param d the table used to plot
#' @return a histogram of the interventions
create_interventions_histogram <- function(d) {

  d <- d|> as.data.frame()

  d$phase[is.na(d$phase)] <- "NA"

  data <- left_join(as.data.frame(d), as.data.frame(interventions), by="nct_id")

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

#' @title Query keywords in brief title from a data set
#' @description query keywords in brief title from database table
#' @param studies the database table
#' @param kw key words
#' @return a tibble with the selected values
title_kw_search <- function(studies, kw) {
  query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    collect()
}

