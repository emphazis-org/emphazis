testthat::test_that("Prepare table", {

  # dput(head(position_table_table))
  position_table_head <- structure(list(x_center = c(30.3260183299389, 31.0634696067909,
                                                     31.6142359620247, 32.4083863368669, 34.536563521562, 33.27543493389
  ), y_center = c(226.04215885947, 225.135086349888, 228.340298649874,
                  231.634204946996, 237.499395459227, 234.291904430527)), row.names = c(NA,
                                                                                        -6L), class = c("tbl_df", "tbl", "data.frame"
                                                                                        ), emphazis_version = structure(list(c(0L, 0L, 0L, 9007L)), class = c("package_version",
                                                                                                                                                              "numeric_version")), unit = "px", fps = 3, analysis_date = "210309-141548-UTC", arena_width = 365L, arena_height = 410L)

  metrics_table_test <- calculate_metrics(
    position_table = position_table_head
  )
  testthat::expect_equal(dplyr::pull(metrics_table_test, distance)[1], 0)

  testthat::expect_equal(ncol(metrics_table_test), 9)
})
