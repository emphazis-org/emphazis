testthat::test_that("Image slice", {

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")

  frames_output <- emphazis::convert_video_to_image(
    video_path = video_path,
    frames_path = fs::path_temp("frame_1"),
    fps = 0.2
  )

  coord1 <- c(263, 12)
  coord2 <- c(670, 461)

  image_path <- frames_output[1]

  sliced_path <- slice_image(
    image_path = image_path,
    output_path = fs::path_temp("slice_1"),
    coord1 = coord1,
    coord2 = coord2
  )

  test_image <- EBImage::readImage(sliced_path)


  testthat::expect_equal(dim(test_image@.Data), c(408, 450, 3))
})


testthat::test_that("Image slice data", {

  video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")

  frames_vector <- emphazis::convert_video_to_image(
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
