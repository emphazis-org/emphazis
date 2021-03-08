#' Plot 3d Track
#'
#' @export
plot_3d_dots <- function(metrics_table, size = 3) {
  `%>%` <- dplyr::`%>%`

  plot_3d_track <- plotly::plot_ly(
    data = metrics_table,
    x = ~x_center, y = ~y_center, z = ~count,
    color = ~count,
    type = "scatter3d",
    mode = "markers",
    size = size
  )
  return(plot_3d_track)

}

#' Plot 3d Track Lines
#'
#' @export
plot_3d_lines <- function(metrics_table) {
  `%>%` <- dplyr::`%>%`

  plot_3d_track <- plotly::plot_ly(
    data = metrics_table,
    x = ~x_center,
    y = ~y_center,
    z = ~count,
    color = ~count
  ) %>%
    plotly::add_paths(color = ~count)
  return(plot_3d_track)

}

#' Plot 3d Track Surface
#'
#' @export
plot_3d_surface <- function(metrics_table) {
  `%>%` <- dplyr::`%>%`

   kernel_density <- MASS::kde2d(
    metrics_table$x_center, metrics_table$y_center, n = 200
  )

  plot_3d_track <- plotly::plot_ly(
    x = kernel_density$x, y = kernel_density$y, z = kernel_density$z
  ) %>%
    plotly::add_surface()
  return(plot_3d_track)
}
