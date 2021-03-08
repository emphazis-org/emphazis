testthat::test_that("Prepare table", {
  res_df_head <- structure(list(x_center = c(
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
    res_df = res_df_head,
    conversion_rate = c(0.045, 0.055),
    fps = 5
  )
  testthat::expect_equal(dplyr::pull(metrics_table_test, distance)[1], 0)

  testthat::expect_equal(ncol(metrics_table_test), 9)
})

testthat::test_that("Summary output", {
  metrics_table_head <- structure(list(frame = 1:6, time = c(
    0, 0.333333333333333, 0.666666666666667,
    1, 1.33333333333333, 1.66666666666667
  ), distance = c(
    0, 1.16902306366098,
    3.25218841182944, 3.38828769618046, 6.23935877107708, 3.44651186184492
  ), x_center = c(
    30.3260183299389, 31.0634696067909, 31.6142359620247,
    32.4083863368669, 34.536563521562, 33.27543493389
  ), y_center = c(
    226.04215885947,
    225.135086349888, 228.340298649874, 231.634204946996, 237.499395459227,
    234.291904430527
  ), dist_cm = c(
    0, 0.0593699742125284, 0.16516555416788,
    0.172077488802393, 0.316871908567397, 0.175034459730939
  ), cumulative_distance_cm = c(
    0,
    0.0593699742125284, 0.224535528380408, 0.396613017182802, 0.713484925750199,
    0.888519385481138
  ), speed = c(
    0, 0.178109922637585, 0.49549666250364,
    0.51623246640718, 0.950615725702192, 0.525103379192817
  ), mov_avg_speed = c(
    NA,
    NA, 0.428090955450119, 0.533111631288683, 0.577036173846807,
    0.568255108094165
  ), count = c(30L, 30L, 30L, 30L, 30L, 30L)), row.names = c(
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
