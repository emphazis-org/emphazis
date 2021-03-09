#' Full matrix from sparse columns
#'
#' Extract full matrix from sparse matrix contained on data.frame columns.
#'
#' @export
extract_matrix <- function(
  data_df,
  x_col = "x_center",
  y_col = "y_center",
  val_col = "count"
) {
  `%>%` <- dplyr::`%>%`
  .data <- rlang::.data
  sparse_mat_df <- data_df %>%
    dplyr::rename(
      x = {{ x_col }},
      y = {{ y_col }},
      z = {{ val_col }}
    ) %>%
    dplyr::select(c(.data[["x"]], .data[["y"]], .data[["z"]])) %>%
    dplyr::mutate(
      x = base::as.integer(.data[["x"]]),
      y = base::as.integer(.data[["y"]])
    ) %>%
    dplyr::arrange(.data[["y"]]) %>%
    dplyr::distinct()

  max_x <- max(dplyr::pull(sparse_mat_df, .data[["x"]]))
  max_y <- max(dplyr::pull(sparse_mat_df, .data[["y"]]))

  full_mat <- matrix(data = 0, nrow = max_y, ncol = max_x)

  for (i in seq_len(nrow(sparse_mat_df))) {
    full_mat[sparse_mat_df$y[i], sparse_mat_df$x[i]] <- sparse_mat_df$z[i]
  }
  return(full_mat)
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
