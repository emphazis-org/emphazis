#' Calculate Distances
#'
#' Calculate distances and cumulative distances from frame to frame.
#' @param res_df Output from video processing.
#' @param conversion_rate Default: NULL. Value of unit conversion rate.
#'   If rate values differ from width and height, a length two numeric vector
#'   with conversion rates for width and lengths can be supplied.
#' @inheritParams proccess_video
#' @export
calculate_metrics <- function(
  res_df,
  fps = 5,
  conversion_rate = NULL
) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  frame_time <- 1 / fps

  # TODO Define pixel to centimeter conversion rate value
  pixel_to_unit <- function(x, conversion_rate) {
    x * conversion_rate
  }

  mov_avg <- function(x, n = 5) {
    stats::filter(x, base::rep(1 / n, n), sides = 2)
  }

  if (is.null(conversion_rate)) {
    width_conversion_rate <- 1
    heigth_conversion_rate <- 1
  } else if (isTRUE(length(conversion_rate == 1))) {
    width_conversion_rate <- conversion_rate
    heigth_conversion_rate <- conversion_rate
  } else {
    width_conversion_rate <- conversion_rate[1]
    heigth_conversion_rate <- conversion_rate[2]
  }

  res_df <- res_df %>%
    dplyr::mutate(x_center = pixel_to_unit(x_center, width_conversion_rate)) %>%
    dplyr::mutate(y_center = pixel_to_unit(y_center, heigth_conversion_rate))

  dist_vector <- NULL
  for (i in 2:nrow(res_df)) {
    dist_vector <- c(dist_vector, sqrt(sum((res_df[i - 1, ] - res_df[i, ])^2)))
  }

  metrics_table <- tibble::tibble(
    time = c(0, seq(1, length(dist_vector)) / fps),
    distance = c(0, dist_vector)
  )

  metrics_table <- res_df %>%
    dplyr::bind_cols(metrics_table) %>%
    tibble::rowid_to_column(var = "frame") %>%
    #dplyr::mutate(dist_cm = pixel_to_unit(.data$distance)) %>%
    dplyr::mutate(cumulative_distance_cm = base::cumsum(.data$distance)) %>%
    dplyr::mutate(speed = .data$distance / frame_time) %>%
    dplyr::mutate(mov_avg_speed = mov_avg(.data$speed)) %>%
    # dplyr::mutate(count_square = count_area_square(metrics_table, 50)) %>%
    # TODO 5 pixel around subject is a good area around subject?
    dplyr::mutate(count = count_area_circle(., 5 * width_conversion_rate))

  return(metrics_table)
}

#' Analysis summary
#'
#' @description prepare table summaries
#'
#' @param metrics_table Output from `calculate_metrics()`.
#' @param unit Unit to convert values from pixel.
#'
#' @export
analysis_summary <- function(
  metrics_table,
  unit = "cm"
) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data
  summary_table <- metrics_table %>%
    dplyr::summarise(
      `Distance traveled (cm)` = sum(.data$distance),
      # `Distance traveled (cm)` = sum(.data$dist_cm),
      `Average Speed (cm/s)` = mean(sum(.data$speed)/sum(.data$distance)),
      `Total time (s)` = max(.data$time),
      `Number of frames` = dplyr::n_distinct(.data$frame)
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
#' @param metrics_table Table containing position of the mass center of the object
#'   per frame.
#' @param side_px Area side length measured in pixels.
#' @export
count_area_square <- function(metrics_table, side_px = 50) {
  `%>%` <- dplyr::`%>%`
  count_vector <- purrr::map_int(seq_len(nrow(metrics_table)), ~{
    i <- .x
    center_x <- dplyr::pull(metrics_table, "x_center")[i]
    center_y <- dplyr::pull(metrics_table, "y_center")[i]
    min_x <- center_x - side_px/2
    max_x <- center_x + side_px/2
    min_y <- center_y - side_px/2
    max_y <- center_y + side_px/2

    area_count <- metrics_table %>%
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
#' @param metrics_table Table containing position of the mass center of the object
#'   per frame.
#' @param diameter Circular area diameter around object.
#' @export
count_area_circle <- function(metrics_table, diameter = 5) {
  `%>%` <- dplyr::`%>%`

  calc_dist <- function(x1, y1, x2, y2) {
    base::sqrt(((x1 - x2)^2) + ((y2 - y1) ^ 2))
  }
  count_vector <- purrr::map_int(seq_len(nrow(metrics_table)), ~{
    i <- .x
    center_x <- dplyr::pull(metrics_table, "x_center")[i]
    center_y <- dplyr::pull(metrics_table, "y_center")[i]
    dist_vec <- purrr::map_dbl(seq_len(nrow(metrics_table)), ~{
      j <- .x
      j_dist <- calc_dist(
        x1 = center_x, x2 = metrics_table$x_center[j],
        y1 = center_y, y2 = metrics_table$y_center[j]
      )
    })
    area_count <- length(dist_vec[dist_vec <= (diameter/2)])
    return(area_count)
  })
  return(count_vector)
}
