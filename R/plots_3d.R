#' Plot 3d Track
#'
#' @export
plot_3d_dots <- function(metrics_table, size = 3) {
  `%>%` <- dplyr::`%>%`

  unit_to_use <- attributes(metrics_table)$unit

  plot_3d_track <- plotly::plot_ly(
    data = metrics_table,
    x = ~x_center,
    y = ~y_center,
    z = ~count,
    color = ~count,
    type = "scatter3d",
    mode = "markers",
    size = size
  ) %>%
    plotly::layout(
      title = "",
      scene = list(
        xaxis = list(title = glue::glue("X ({ unit_to_use })")),
        yaxis = list(title = glue::glue("Y ({ unit_to_use })")),
        zaxis = list(title = "Proximity score")
      )
    )
  return(plot_3d_track)

  fig %>%
    plotly::layout(
    title = "Layout options in a 3d scatter plot",
    scene = list(
      xaxis = list(title = "Cos"),
      yaxis = list(title = "Sin"),
      zaxis = list(title = "Z")
    ))

}

#' Plot 3d Track Lines
#'
#' @export
plot_3d_lines <- function(metrics_table) {
  `%>%` <- dplyr::`%>%`

  unit_to_use <- attributes(metrics_table)$unit

  plot_3d_track <- plotly::plot_ly(
    data = metrics_table,
    x = ~x_center,
    y = ~y_center,
    z = ~count,
    color = ~count
  ) %>%
    plotly::add_paths(color = ~count) %>%
    plotly::layout(
      title = "",
      scene = list(
        xaxis = list(title = glue::glue("X ({ unit_to_use })")),
        yaxis = list(title = glue::glue("Y ({ unit_to_use })")),
        zaxis = list(title = "Proximity score")
      )
    )
  return(plot_3d_track)

}

#' Plot 3d Track Surface
#'
#' @export
plot_3d_surface <- function(metrics_table) {
  `%>%` <- dplyr::`%>%`

  unit_to_use <- attributes(metrics_table)$unit

   kernel_density <- MASS::kde2d(
    metrics_table$x_center, metrics_table$y_center, n = 200
  )

  plot_3d_track <- plotly::plot_ly(
    x = kernel_density$x,
    y = kernel_density$y,
    z = kernel_density$z
  ) %>%
    plotly::add_surface() %>%
    plotly::layout(
      title = "",
      scene = list(
        xaxis = list(title = glue::glue("X ({ unit_to_use })")),
        yaxis = list(title = glue::glue("Y ({ unit_to_use })")),
        zaxis = list(title = "Kernel density")
      )
    )
  return(plot_3d_track)
}
