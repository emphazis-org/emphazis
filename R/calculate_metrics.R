#' Calculate Metrics
#'
#' @description Calculate distances traveled, cumulative distances, time,
#'   and other metrics from frame to frame.
#' @param position_table Coordinate position of subject per frame.
#' @return tibble with additional attributes.
#' @export
calculate_metrics <- function(
  position_table
) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  fps <- as.numeric(attributes(position_table)$fps)

  frame_time <- 1 / fps

  mov_avg <- function(x, n = 5) {
    stats::filter(x, base::rep(1 / n, n), sides = 2)
  }

  dist_vector <- NULL
  for (i in 2:nrow(position_table)) {
    dist_vector <- c(
      dist_vector, sqrt(sum((position_table[i - 1, ] - position_table[i, ])^2))
    )
  }

  metrics_table <- tibble::tibble(
    time = c(0, seq(1, length(dist_vector)) / fps),
    distance = c(0, dist_vector)
  )

  metrics_table <- position_table %>%
    dplyr::mutate(time = metrics_table$time) %>%
    dplyr::mutate(distance = metrics_table$distance) %>%
    tibble::rowid_to_column(var = "frame") %>%
    dplyr::mutate(cumulative_distance = base::cumsum(.data$distance)) %>%
    dplyr::mutate(speed = .data$distance / frame_time) %>%
    dplyr::mutate(mov_avg_speed = mov_avg(.data$speed))

  # TODO is 5 pixel around subject is a good area around subject?
  counts_vector <- count_area_circle(
    metrics_table = metrics_table,
    diameter_pct = 5
  )

  metrics_table <- metrics_table %>%
    dplyr::mutate(count = counts_vector)
  return(metrics_table)
}
