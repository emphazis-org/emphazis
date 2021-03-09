testthat::test_that("Summary output", {
  # dput(metrics_table_test)
  metrics_table_head <- structure(list(frame = 1:6, x_center = c(
    30.3260183299389, 31.0634696067909,
    31.6142359620247, 32.4083863368669, 34.536563521562, 33.27543493389
  ), y_center = c(
    226.04215885947, 225.135086349888, 228.340298649874,
    231.634204946996, 237.499395459227, 234.291904430527
  ), time = c(
    0,
    0.333333333333333, 0.666666666666667, 1, 1.33333333333333, 1.66666666666667
  ), distance = c(
    0, 1.16902306366042, 3.25218841182964, 3.38828769617958,
    6.23935877107748, 3.44651186184445
  ), cumulative_distance = c(
    0,
    1.16902306366042, 4.42121147549006, 7.80949917166964, 14.0488579427471,
    17.4953698045916
  ), speed = c(
    0, 3.50706919098126, 9.75656523548892,
    10.1648630885387, 18.7180763132324, 10.3395355855334
  ), mov_avg_speed = structure(c(
    NA,
    NA, 8.42931476564827, 10.4972218827549, NA, NA
  ), .Tsp = c(
    1,
    6, 1
  ), class = "ts"), count = c(2L, 2L, 1L, 1L, 1L, 1L)), row.names = c(
    NA,
    -6L
  ), class = c("tbl_df", "tbl", "data.frame"), emphazis_version = structure(list(
    c(0L, 0L, 0L, 9007L)
  ), class = c("package_version", "numeric_version")), unit = "px", fps = 3, analysis_date = "210309-135137-UTC")


  summary_df_test <- analysis_summary(
    metrics_table = metrics_table_head
  )

  testthat::expect_equal(ncol(summary_df_test), 2)

  testthat::expect_equal(nrow(summary_df_test), 4)

  testthat::expect_equal(colnames(summary_df_test), c("var", "value"))


  convert_table_unit(
    metrics_table = metrics_table_head,
    conversion_rate = c(0.045, 0.032),
    unit = "cm"
  ) %>%
    analysis_summary() %>%
    dplyr::pull("var") %>%
    stringr::str_detect("cm") %>%
    testthat::expect_equal(c(TRUE, TRUE, FALSE, FALSE))
})
