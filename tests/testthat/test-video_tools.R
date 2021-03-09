testthat::test_that("Process video", {
  subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
  background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
  model_test <- generate_subject_model(subject_path, background_path)

  testthat::expect_s3_class(model_test, "glm")

  testthat::expect_equal(model_test$family$family, "binomial")

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
  temp_frames_path <- fs::path_temp("frames")

  coord1 <- c(285, 20)

  coord2 <- c(650, 430)

  progressr::with_progress({
    position_table_table <- proccess_video(
      video_path = video_path,
      frames_path = temp_frames_path,
      subject_model = model_test,
      coord1 = coord1,
      coord2 = coord2,
      fps = 3
    )
  })
  testthat::expect_equal(
    nrow(position_table_table),
    length(fs::dir_ls(temp_frames_path))
  )

  testthat::expect_gt(position_table_table$y_center[2], 100)

  metrics_table <- calculate_metrics(position_table_table)

  testthat::expect_equal(metrics_table$mov_avg_speed[1], NA_integer_)
  testthat::expect_equal(
    nrow(metrics_table),
    length(fs::dir_ls(temp_frames_path))
  )

  # coords bigger than image handled correctly
  position_table_test_2 <- proccess_video(
    video_path = video_path,
    frames_path = temp_frames_path,
    subject_model = model_test,
    coord1 = c(0, 0),
    coord2 = c(1000, 1000)
  )
  testthat::expect_equal(
    dim(position_table_test_2), c(50, 2)
  )
})
