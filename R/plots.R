
#' Plot Cumulative distance
#' Plot cumulative sum of distance traveled by the subject.
#' @export
plot_cumulative_distance <- function(dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`

  begin_x_axis <- min(dist_table$time)
  end_x_axis <- max(dist_table$time)

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  dist_table %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = time, y = cumulative_distance_cm)) +
    ggplot2::expand_limits(x = c(begin_x_axis, end_x_axis)) +
    ggplot2::theme_bw()
}

#' Plot average speed
#' Plot cumulative sum of distance traveled by the subject.
#' @export
plot_average_speed <- function(dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`

  begin_x_axis <- min(dist_table$time)
  end_x_axis <- max(dist_table$time)

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  dist_table %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = time, y = mov_avg_speed)) +
    ggplot2::geom_point(ggplot2::aes(x = time, y = mov_avg_speed)) +
    ggplot2::expand_limits(x = c(begin_x_axis, end_x_axis)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_bw()
}

#' Plot tracking path
#' @export
plot_track <- function(path_table, dist_table, color_var = "red", range = NULL) {
  `%>%` <- dplyr::`%>%`

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])

    path_table <- path_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  plot_res <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = dist_table,
      mapping = ggplot2::aes(x = y_center, y = x_center),
      color = color_var
    ) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.position = "none")
  return(plot_res)
}

#' Plot tracking path density
#' @export
plot_track_heatmap <- function(path_table, dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])

    path_table <- path_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }
  plot_res <- dist_table %>%
    ggplot2::ggplot(ggplot2::aes(x = y_center, y = x_center) ) +
    ggplot2::stat_density_2d(ggplot2::aes(fill = ..density..), geom = "raster", contour = FALSE) +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none")
  return(plot_res)
}

