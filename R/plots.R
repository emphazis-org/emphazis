
#' Plot Cumulative distance
#' Plot cumulative sum of distance traveled by the subject.
#' @export
plot_cumulative_distance <- function(dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  begin_x_axis <- min(dist_table$time)
  end_x_axis <- max(dist_table$time)

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  dist_table %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(
      x = .data[["time"]],
      y = .data[["cumulative_distance_cm"]]
    )) +
    ggplot2::expand_limits(x = c(begin_x_axis, end_x_axis)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme_bw()
}

#' Plot average speed
#' Plot cumulative sum of distance traveled by the subject.
#' @export
plot_average_speed <- function(dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  begin_x_axis <- min(dist_table$time)
  end_x_axis <- max(dist_table$time)

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  dist_table %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data[["time"]],
        y = .data[["mov_avg_speed"]]
      )
    ) +
    ggplot2::geom_smooth() +
    ggplot2::geom_point() +
    ggplot2::expand_limits(x = c(begin_x_axis, end_x_axis)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme_bw()
}

#' Plot tracking path
#' @export
plot_track <- function(
  dist_table, color = "purple", range = NULL
) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  plot_res <- dist_table %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data[["x_center"]],
        y = .data[["y_center"]]
      ),
    ) +
    ggplot2::geom_path(color = color) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme(legend.position = "none")

  return(plot_res)
}

#' Plot tracking path density
#' @export
plot_track_heatmap <- function(dist_table, range = NULL) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  if (!is.null(range)) {
    dist_table <- dist_table %>%
      dplyr::filter(frame >= range[1] & frame <= range[2])
  }

  plot_res <- dist_table %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data[["x_center"]],
        y = .data[["y_center"]]
      )) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ..density..),
      geom = "raster", contour = FALSE
    ) +
    ggplot2::scale_fill_viridis_c(option = "viridis", direction = 1) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme(legend.position = "none")
  return(plot_res)
}

