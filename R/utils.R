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
