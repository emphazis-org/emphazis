
#' Calculate Distances
#'
#' Calculate distances and cumulative distances from frame to frame.
#' @param res_df Output from video processing.
#' @export
calculate_distances <- function(res_df, fps = 3) {
  `%>%` <- dplyr::`%>%`

  dist <- NULL
  for (i in 2:nrow(res_df)) {
    dist <- c(dist, sqrt(sum((res_df[i - 1, ] - res_df[i, ])^2)))
  }

  frame_time <- 1 / fps

  # TODO definir esse valor de conversao
  pixel_to_cm <- function(x, conversion_rate = 21 / 413.5) {
    x * conversion_rate
  }
  mov_avg <- function(x, n = 5) {
    stats::filter(x, base::rep(1 / n, n), sides = 2)
  }

  dist_table <- tibble::tibble(
    time = c(0, seq(1, length(dist)) / fps),
    distance = c(0, dist)
  ) %>%
    dplyr::bind_cols(res_df) %>%
    tibble::rowid_to_column(var = "frame") %>%
    dplyr::mutate(dist_cm = pixel_to_cm(distance)) %>%
    dplyr::mutate(cumulative_distance_cm = base::cumsum(dist_cm)) %>%
    dplyr::mutate(speed = dist_cm / frame_time) %>%
    dplyr::mutate(mov_avg_speed = mov_avg(speed)) %>%
    # dplyr::mutate(count_square = count_area_square(dist_table, 50)) %>%
    dplyr::mutate(count = count_area_circle(., 50))
  return(dist_table)
}

#' Analysis summary
#'
#' @description prepare table summaries
#'
#' @param dist_table Output from `calculate_distances`.
#'
#' @export
analysis_summary <- function(dist) {
  summary_table <- dist_table %>%
    dplyr::summarise(
      `Distance Pixel` = sum(distance),
      `Distance cm` = sum(dist_cm),
      `Average Speed cm/s` = mean(sum(speed)/sum(distance)),
      `Number of frames` = dplyr::n_distinct(frame)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "var"
    )

  return(summary_table)
}

#' Count occurrences in square area
#'
#' Calculates how many times other points
#'   happens in an area around the main point.
#'
#' @param dist_table Table containing position of the mass center of the object
#'   per frame.
#' @param side_px Area side length measured in pixels.
#' @export
count_area_square <- function(dist_table, side_px = 50) {
  `%>%` <- dplyr::`%>%`
  count_vector <- purrr::map_int(seq_len(nrow(dist_table)), ~{
    i <- .x
    center_x <- dplyr::pull(dist_table, "x_center")[i]
    center_y <- dplyr::pull(dist_table, "y_center")[i]
    min_x <- center_x - side_px/2
    max_x <- center_x + side_px/2
    min_y <- center_y - side_px/2
    max_y <- center_y + side_px/2

    area_count <- dist_table %>%
      dplyr::filter(
        x_center >= min_x & x_center <= max_x
      ) %>%
      dplyr::filter(
        y_center >= min_y & y_center <= max_y
      ) %>%
      nrow()
    return(area_count)
  })
  return(count_vector)
}

#' Count occurrences in circular area
#'
#' Calculates how many times other points
#'   happens in an area around the main point.
#'
#' @param dist_table Table containing position of the mass center of the object
#'   per frame.
#' @param diameter_px Area side length measured in pixels.
#' @export
count_area_circle <- function(dist_table, diameter_px = 50) {
  `%>%` <- dplyr::`%>%`
  calc_dist <- function(x1, y1, x2, y2) {
    base::sqrt(((x1 - x2)^2) + ((y2 - y1) ^ 2))
  }
  count_vector <- purrr::map_int(seq_len(nrow(dist_table)), ~{
    i <- .x
    center_x <- dplyr::pull(dist_table, "x_center")[i]
    center_y <- dplyr::pull(dist_table, "y_center")[i]
    dist_vec <- purrr::map_dbl(seq_len(nrow(dist_table)), ~{
      j <- .x
      j_dist <- calc_dist(
        x1 = center_x, x2 = dist_table$x_center[j],
        y1 = center_y, y2 = dist_table$y_center[j]
      )
    })
    area_count <- length(dist_vec[dist_vec <= (diameter_px/2)])
    return(area_count)
  })
  return(count_vector)
}
