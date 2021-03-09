testthat::test_that("Image slice", {

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")

  frames_output <- convert_video_to_image(
    video_path = video_path,
    frames_path = fs::path_temp("frame_test_1"),
    fps = 0.2
  )

  coord1 <- c(263, 12)
  coord2 <- c(670, 461)

  image_path <- frames_output[1]

  sliced_path <- slice_image(
    image_path = image_path,
    output_path = fs::path_temp("slice_test_1"),
    coord1 = coord1,
    coord2 = coord2
  )

  test_image <- EBImage::readImage(sliced_path)

  # plot(test_image)

  testthat::expect_equal(dim(test_image@.Data), c(408, 450, 3))
})

testthat::test_that("Image slice data", {

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")

  frames_vector <- convert_video_to_image(
    video_path = video_path,
    frames_path = fs::path_temp("frame_1"),
    fps = 0.2
  )

  first_frame_path <- frames_vector[1]

  first_frame <- EBImage::readImage(first_frame_path)


  frame_data <- first_frame@.Data

  arena_xmax <- dim(frame_data)[1]
  arena_ymax <- dim(frame_data)[2]

  arena_frame <- first_frame
  # 1st dim = X starting from left
  # 2nd dim = Y starting from top
  arena_frame@.Data <- frame_data[0:arena_xmax, 0:(arena_ymax - 200), ]
  #plot(arena_frame)

  testthat::expect_equal(dim(arena_frame@.Data), c(848, 280, 3))

})

testthat::test_that("Full analysis with automated slices", {

  `%>%` <- dplyr::`%>%`

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")

  frames_output <- convert_video_to_image(
    video_path = video_path,
    frames_path = fs::path_temp("slice_test_2"),
    fps = 0.2
  )

  first_frame_test <- frames_output[1]

  # arena
  arena_coord_1 <- c(287, 3)
  arena_coord_2 <- c(695, 474)

  sliced_arena_path <- slice_image(
    image_path = first_frame_test,
    output_path = fs::path_temp("slice_test_1"),
    coord1 = arena_coord_1,
    coord2 = arena_coord_2
  )

  arena_test_image <- EBImage::readImage(sliced_arena_path)

  # plot(arena_test_image)


  # subject1
  subject_1_coord_1 <- c(293, 192)
  subject_1_coord_2 <- c(303, 211)
  sliced_subject_path <- slice_image(
    image_path = sliced_arena_path,
    output_path = fs::path_temp("slice_test_1"),
    coord1 = subject_1_coord_1,
    coord2 = subject_1_coord_2
  )

  subject_test_image <- EBImage::readImage(sliced_subject_path)

  # plot(subject_test_image)

  # generate model
  subject_model_from_slices <- generate_subject_model(
    subject_path = sliced_subject_path,
    background_path = sliced_arena_path
  )

  arena_width_px <- av::av_media_info(sliced_arena_path)$video$width
  arena_height_px <- av::av_media_info(sliced_arena_path)$video$height

  # run analysis
  temp_frames_path <- fs::path_temp("frames_test_1")
  frames_output_test <- proccess_video(
    video_path = video_path,
    frames_path = temp_frames_path,
    subject_model = subject_model_from_slices,
    coord1 = arena_coord_1,
    coord2 = arena_coord_2,
    fps = 5
  )

  fs::dir_info(temp_frames_path)

  summary_test <- analysis_summary(
    calculate_metrics(frames_output_test)
  )

  summary_test %>%
    dplyr::filter(var %in% "Number of frames") %>%
    dplyr::pull("value") %>%
    testthat::expect_equal(46)

  plot_test_x <- plot_track(calculate_metrics(frames_output_test))
  testthat::expect_s3_class(plot_test_x, "ggplot")
})


testthat::test_that("Missing conversion inputs", {

  testthat::expect_error(
    convert_image_size_unit(),
    regexp = "Width and height need to be supplied"
  )
  testthat::expect_error(
    convert_image_size_unit(
      width = NULL,
      height = NULL
    ),
    regexp = "Optionally, a dpi value can be supplied instead"
  )
})


testthat::test_that("Conversion values", {

  image_path <- fs::path_package("emphazis", "extdata", "background.jpg")

  conversion_rates <- convert_image_size_unit(
    image_path = image_path,
    width = 21,
    height = 21
  )

  testthat::expect_equal(round(conversion_rates, 3), c(0.051, 0.044))

})
