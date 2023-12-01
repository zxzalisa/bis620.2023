#' Check Environment
#'
#' Check if the required database is available,
#' otherwise install from remote repository.
#'
#' @return a database connection either connected with local file or remote one.
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom devtools install_github
#' @export
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
    devtools::install_github("presagia-analytics/ctrialsgov")

    # Load the database after ensuring it's installed
    con <- dbConnect(duckdb(
      file.path("ctrialsgovdb/ctrialsgov.duckdb"),
      read_only = TRUE
    ))
  }

  # Return any necessary objects or connections
  # For example, return the DuckDB connection if needed elsewhere
  return(con)
}
