#' @title Phase Histogram
#' @description create a histogram of the phase
#'
#' @param d the table used to plot
#' @return a histogram of the phase
#' @importFrom dplyr left_join select filter group_by summarize
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @importFrom tibble add_row
#' @export
create_phase_histogram_plot <- function(d) {
  d$phase[is.na(d$phase)] <- "NA"

  d <- d |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  #allphase is the character contains the name of all types of phases
  allphase<- studies |> select(phase) |> as_tibble() |>unique()
  allphase<-as.character(allphase$phase)
  allphase[is.na(allphase)] <- "NA"

  #allphase<-as.character("NA","Phase1")

  dnew <- d
  #we reorder the rows in d, and add n=0 for the phases do not occur in d.
  for(i in 1:length(allphase)){
    if(sum(d$phase == as.character(allphase)[i])>0){
      dnew[i,]<- d[d$phase==as.character(allphase)[i],]
    }else{
      dnew[i,]<- d[as.character(allphase)[i],0]
    }
  }

  ggplot(dnew, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}
