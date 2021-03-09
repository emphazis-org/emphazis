testthat::test_that("Prepare table", {
  position_table_head <- structure(list(x_center = c(
    30.3260183299389, 31.0634696067909,
    31.6142359620247, 32.4083863368669, 34.536563521562, 33.27543493389
  ), y_center = c(
    226.04215885947, 225.135086349888, 228.340298649874,
    231.634204946996, 237.499395459227, 234.291904430527
  )), row.names = c(
    NA,
    -6L
  ), class = c("tbl_df", "tbl", "data.frame"))

  metrics_table_test <- calculate_metrics(
    position_table = position_table_head,
    conversion_rate = c(0.045, 0.055),
    fps = 5
  )
  testthat::expect_equal(dplyr::pull(metrics_table_test, distance)[1], 0)

  testthat::expect_equal(ncol(metrics_table_test), 9)
})

testthat::test_that("Summary output", {
  # dput(metrics_table_test)
  metrics_table_head <- structure(list(frame = 1:6, x_center = c(
    1.36467082484725, 1.39785613230559,
    1.42264061829111, 1.45837738515901, 1.55414535847029, 1.49739457202505
  ), y_center = c(
    12.4323187372708, 12.3824297492438, 12.5587164257431,
    12.7398812720848, 13.0624667502575, 12.886054743679
  ), time = c(
    0,
    0.2, 0.4, 0.6, 0.8, 1
  ), distance = c(
    0, 0.0599180753818376, 0.178020400675626,
    0.184655945087577, 0.336500959047761, 0.185315535849549
  ), cumulative_distance_cm = c(
    0,
    0.0599180753818376, 0.237938476057464, 0.42259442114504, 0.759095380192801,
    0.94441091604235
  ), speed = c(
    0, 0.299590376909188, 0.89010200337813,
    0.923279725437883, 1.6825047952388, 0.926577679247745
  ), mov_avg_speed = structure(c(
    NA,
    NA, 0.759095380192801, 0.94441091604235, NA, NA
  ), .Tsp = c(
    1,
    6, 1
  ), class = "ts"), count = c(2L, 2L, 1L, 1L, 1L, 1L)), row.names = c(
    NA,
    -6L
  ), class = c("tbl_df", "tbl", "data.frame"))

  summary_df_test <- analysis_summary(
    metrics_table = metrics_table_head
  )


  testthat::expect_equal(ncol(summary_df_test), 2)

  testthat::expect_equal(nrow(summary_df_test), 4)

  testthat::expect_equal(colnames(summary_df_test), c("var", "value"))
})
