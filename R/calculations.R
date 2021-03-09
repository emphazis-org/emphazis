#' Convert Unit used by Metrics Table
#'
#' @description Convert units used to calculate metrics.
#'
#' @param metrics_table Output from `calculate_metrics()`.
#' @param conversion_rate_width Default: NULL. Value of unit conversion rate.
#'   If rate values differ from width and height, a length two numeric vector
#'   with conversion rates for width and lengths can be supplied.
#' @param conversion_rate_height Default: NULL. Value of unit conversion rate.
#'   If rate values differ from width and height, a length two numeric vector
#'   with conversion rates for width and lengths can be supplied.
#' @param unit Unit to convert values from pixel.
#'
#' @export
convert_table_unit <- function(
                               metrics_table,
                               conversion_rate_width = NULL,
                               conversion_rate_height = NULL,
                               unit = "cm") {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  # Define pixel to centimeter conversion rate value
  pixel_to_unit <- function(x, conversion_rate) {
    x * conversion_rate
  }
  position_table <- metrics_table %>%
    dplyr::mutate(
      x_center = pixel_to_unit(.data$x_center, conversion_rate_width)
    ) %>%
    dplyr::mutate(
      y_center = pixel_to_unit(.data$y_center, conversion_rate_height)
    ) %>%
    dplyr::select(c("x_center", "y_center"))

  attr(position_table, "unit") <- unit

  converted_table <- calculate_metrics(position_table = position_table)

  return(converted_table)
}

#' Analysis summary
#'
#' @description prepare table summaries
#'
#' @param metrics_table Output from `calculate_metrics()`
#'   or `convert_table_unit()`.
#'
#' @export
analysis_summary <- function(
                             metrics_table) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data

  unit_to_replace <- attr(metrics_table, "unit")

  summary_table <- metrics_table %>%
    dplyr::summarise(
      `Distance traveled (unit_to_print)` = sum(.data$distance),
      `Average Speed (unit_to_print/s)` = mean(sum(.data$speed) / sum(.data$distance)),
      `Total time (s)` = max(.data$time),
      `Number of frames` = dplyr::n_distinct(.data$frame)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "var"
    )

  var_with_unit <- stringr::str_replace(
    string = summary_table$var,
    pattern = "unit_to_print",
    replacement = unit_to_replace
  )
  summary_table <- dplyr::mutate(summary_table, var = var_with_unit)

  return(summary_table)
}

#' Count occurrences in circular area
#'
#' Calculates how many times other points
#'   happens in an area around the main point.
#'
#' @param metrics_table Table containing position of the mass center of the object
#'   per frame.
#' @param diameter_pct Circular area diameter around object.
#' @export
count_area_circle <- function(metrics_table, diameter_pct = 5) {
  `%>%` <- dplyr::`%>%`

  # circle_area <- base::pi * (diameter / 2) ^ 2
  arena_area <- `*`(
    attr(metrics_table, "arena_width"),
    attr(metrics_table, "arena_height")
  )
  diameter <- (sqrt(arena_area) / 100) * diameter_pct


  calc_dist <- function(x1, y1, x2, y2) {
    base::sqrt(((x1 - x2)^2) + ((y2 - y1)^2))
  }
  count_vector <- purrr::map_int(seq_len(nrow(metrics_table)), ~ {
    i <- .x
    center_x <- dplyr::pull(metrics_table, "x_center")[i]
    center_y <- dplyr::pull(metrics_table, "y_center")[i]
    dist_vec <- purrr::map_dbl(seq_len(nrow(metrics_table)), ~ {
      j <- .x
      j_dist <- calc_dist(
        x1 = center_x, x2 = metrics_table$x_center[j],
        y1 = center_y, y2 = metrics_table$y_center[j]
      )
    })
    area_count <- length(dist_vec[dist_vec <= (diameter / 2)])
    return(area_count)
  })
  return(count_vector)
}
