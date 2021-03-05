#' Convert video to image
#' @inheritParams proccess_video
#' @export
convert_video_to_image <- function(video_path, frames_path, fps = 5) {
  av::av_video_images(
    video = video_path,
    destdir = frames_path,
    format = "jpg",
    fps = fps
  )
}


#' Extract data from video
#'
#' @description Extract data and frames from video.
#' @param video_path Path to video file.
#' @param subject_model model generated from `generate_subject_model()`.
#' @param frames_path Path to save frames extracted from video.
#' @param coord1 length 2 vector with position of the top right point.
#' @param coord2 length 2 vector with position of the bottom left point.
#' @param fps default = 5; Frames per second to decompose video.
#' @export
proccess_video <- function(
   video_path,
   subject_model,
   frames_path,
   coord1 = NULL,
   coord2 = NULL,
   fps = 5
) {
  `%>%` <- dplyr::`%>%`
  if (fs::dir_exists(frames_path)) {
    fs::dir_delete(frames_path)
  }
  frames_vector <- convert_video_to_image(
    video_path = video_path, frames_path = frames_path, fps = fps
  )

  im <- EBImage::readImage(frames_vector[1])

  im2 <- im
  if (isTRUE(is.null(coord1) & is.null(coord2))) {
    maat <- im@.Data
  } else {
    maat <- im@.Data[coord1[1]:coord1[2], coord2[1]:coord2[2], ]
  }
  im2@.Data <- maat

  # Progress bar count
  prog_count <- progressr::progressor(along = frames_vector)

  # frame_path = frames_vector[2]
  extract_values <- function(frame_path) {
    `%>%` <- dplyr::`%>%`
    im <- EBImage::readImage(frame_path)
    if (isTRUE(is.null(coord1) & is.null(coord2))) {
      maat <- im@.Data
    } else {
      maat <- im@.Data[coord1[1]:coord1[2], coord2[1]:coord2[2], ]
    }
    im2@.Data <- maat
    matIm2 <- cbind(
      c(im2@.Data[, , 1]), c(im2@.Data[, , 2]), c(im2@.Data[, , 3])
    )
    colnames(matIm2) <- c("R", "G", "B")
    Pred <- round(stats::predict(
      subject_model, base::data.frame(matIm2),
      type = "response"
    ), 0)
    imM <- matrix(Pred, ncol = ncol(im2@.Data))
    imM <- EBImage::bwlabel(imM)
    imM <- EBImage::fillHull(imM)

    area <- EBImage::computeFeatures.shape(imM)

    base::suppressWarnings({
      id <- area[, 1] == max(area[, 1])
    })

    x_y_center <- EBImage::computeFeatures.moment(imM)[id][1:2]

    res_temp <- tibble::tibble(
      x_center = x_y_center[1],
      y_center = x_y_center[2]
    )

    # Progress counter
    prog_count()
    return(res_temp)
  }

  res_df <- purrr::map_dfr(
    frames_vector,
    extract_values
  )

  return(res_df)
}
