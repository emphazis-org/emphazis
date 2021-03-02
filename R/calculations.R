
#' Calculate Distances
#'
#' Calculate distances and cumulative distances from frame to frame.
#' @param res_list Output from video processing.
#' @export
calculate_distances <- function(res_list) {
  `%>%` <- dplyr::`%>%`
  res_df <- purrr::map_dfr(res_list, function(x) {
    x[[2]]
  })
  dist <- NULL
  for (i in 2:nrow(res_df)) {
    dist <- c(dist, sqrt(sum((res_df[i - 1, ] - res_df[i, ])^2)))
  }

  frame_time <- 1 / 3

  # TODO definir esse valor de conversao
  pixel_to_cm <- function(x, conversion_rate = 21 / 413.5) {
    x * conversion_rate
  }
  mov_avg <- function(x, n = 5) {
    stats::filter(x, base::rep(1 / n, n), sides = 2)
  }

  dist_table <- tibble::tibble(
    time = c(0, seq(1, length(dist)) / 3),
    distance = c(0, dist)
  ) %>%
    dplyr::bind_cols(res_df) %>%
    tibble::rowid_to_column(var = "frame") %>%
    dplyr::mutate(dist_cm = pixel_to_cm(distance)) %>%
    dplyr::mutate(cumulative_distance_cm = base::cumsum(dist_cm)) %>%
    dplyr::mutate(speed = dist_cm / frame_time) %>%
    dplyr::mutate(mov_avg_speed = mov_avg(speed)) %>%
    dplyr::mutate(count = count_area(dist_table, 50))
  return(dist_table)
}

#' Count in area
#'
#' Calcutates how many times other points
#'   happens in an area around the main point.
#'
#' @param dist_table Table containing position of the mass center of the object
#'   per frame.
#' @param side_px Area side length measured in pixels.
#' @export
count_area <- function(dist_table, side_px = 50) {
  `%>%` <- dplyr::`%>%`
  count_vector <- purrr::map_int(seq_len(nrow(dist_table)), ~{
    # for (i in seq_len(nrow(dist_table))) {
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

#' Prepare path data
#'
#' Prepare data for path plotting.
#' @param res_list Output from video processing.
#' @export
prepare_path_data <- function(res_list) {
  path_df <- purrr::map2_dfr(
    .x = res_list,
    .y = seq_along(res_list),
    .f = function(res_list, frame_pos) {
      dplyr::mutate(res_list[[1]], frame = {{ frame_pos }})
    }
  )
  return(path_df)
}
