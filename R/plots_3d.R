#' Plot 3d Track
#'
#' @export
plot_track_3d <- function(dist_table) {
  `%>%` <- dplyr::`%>%`

  plot_3d_track <- plotly::plot_ly(
    data = dist_table,
    x = ~y_center, y = ~x_center, z = ~count,
    color = ~count
  ) # %>%
    # plotly::add_paths(color = ~count)
  plot_3d_track

}
