testthat::test_that("Process video", {
  subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
  background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
  model_test <- generate_subject_model(subject_path, background_path)

  testthat::expect_s3_class(model_test, "glm")

  testthat::expect_equal(model_test$family$family, "binomial")

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
  temp_frames_path <- fs::path_temp("frames")
  # av::av_video_info(video_path)
  # (xmin, ymin)
  coord1 <- c(285, 20)
  # (xmax, ymax)
  coord2 <- c(650, 430)

  progressr::with_progress({
    x_test <- proccess_video(
      video_path = video_path,
      frames_path = temp_frames_path,
      subject_model = model_test,
      coord1 = coord1,
      coord2 = coord2,
      fps = 3
    )
  })
  testthat::expect_equal(nrow(x_test), length(fs::dir_ls(temp_frames_path)))

  testthat::expect_gt(x_test$y_center[2], 100)

  dist_table <- calculate_distances(x_test)

  testthat::expect_equal(dist_table$mov_avg_speed[1], NA_integer_)
  testthat::expect_equal(nrow(dist_table), length(fs::dir_ls(temp_frames_path)))

  y_test <- proccess_video(
    video_path = video_path,
    frames_path = temp_frames_path,
    subject_model = model_test
  )
  testthat::expect_equal(
   dim(y_test), c(50, 2)
  )
})
