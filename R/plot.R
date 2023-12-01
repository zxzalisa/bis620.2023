
#' Plot 3DF Accelerometry Data
#'
#' @param x a tibble with columns time, X, Y, Z.
#' @param x_var the x-axis variable.
#' @returns A ggplot object, faceted by channel, showing the acceleration
#' or energy (in the Fourier domain) of the data.
#' @importFrom ggplot2 ggplot aes geom_line facet_grid
#' @importFrom tidyr pivot_longer
#' @export
plot_accel <- function(x, x_var = "time") {
  x |>
    pivot_longer(-!!x_var, values_to = "Acceleration", names_to = "Channel") |>
    ggplot(aes(x = !!as.symbol(x_var), y = Acceleration)) +
    geom_line() +
    facet_grid(Channel ~ .)
}
