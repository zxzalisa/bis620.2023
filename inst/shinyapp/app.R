#input libraries
library(shiny)
library(duckdb)
library(dplyr)
library(purrr)
library(tidyr)
library(DBI)
library(DT)
library(ggplot2)
library(tm)
library(wordcloud)
library(memoise)
options(warn = -1)

#create connection to database using duckdb and create table
con <- dbConnect(
  duckdb(
    file.path("ctrialsgovdb/ctrialsgov.duckdb"),
    read_only = TRUE
  )
)

dbListTables(con)
studies <- tbl(con, "studies")
sponsors <- tbl(con, "sponsors")
conditions <- tbl(con, "conditions")
interventions <- tbl(con, "interventions")

source("ct-util.R")

# Define user interface
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      numericInput("n_conditions", "Number of Conditions in Histogram",
                   value = 10, min = 1, max = 30),
      selectInput("sponsor", "Sponsor Type",
                choices <- list("ALL" = "ALL",
                                 "Federal" = "FED",
                                 "Individual" = "INDIV",
                                 "Industry" = "INDUSTRY",
                                 "Network" = "NETWORK",
                                 "NIH" = "NIH",
                                 "Other" = "OTHER",
                                 "Other gov" = "OTHER_GOV",
                                 "Unknown" = "UNKNOWN")),
      sliderInput("freq",
                  "Minimum Frequency of Conditions in Word-cloud",
                  min = 1,  max = 50, value = 15),
      sliderInput("max_words",
                  "Maximum Number of Conditions in Word-could",
                  min = 1,  max = 300,  value = 100)
    )
    ,

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase Histogram", plotOutput("distPlot")),
        tabPanel("Condition Histogram", plotOutput("conditionPlot")),
        tabPanel("Condition Word-cloud Plot", plotOutput("cloudPlot")),
        tabPanel("Intervention Histogram", plotOutput("interventionPlot"))
      ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server function
server <- function(input, output) {

  get_studies <- reactive({
    if (input$brief_title_kw != "") {
      si <- input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret <- query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret <- studies
    }
    # Filter sponsor type
    if (input$sponsor != "ALL") {
      ret <- ret |>
        filter(source_class %in% !!input$sponsor)
    }

    ret |>
      head(1000) |>
      collect()

  })

  output$distPlot <- renderPlot({
    get_studies() |>
      create_phase_histogram_plot()
  })

  output$conditionPlot <- renderPlot({
    get_studies() |>
      create_condition_histogram(input$n_conditions)
  })

  output$cloudPlot <- renderPlot({
    get_studies() |>
      create_condition_cloud(input$freq, input$max_words)
  })

  output$interventionPlot <- renderPlot({
    get_studies() |>
      create_interventions_histogram()
  })

  output$trial_table <- renderDataTable({
    si <- trimws(unlist(strsplit(input$brief_title_kw, ",")))
    title_kw_search(studies, si) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date) |>
      head(1000)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
