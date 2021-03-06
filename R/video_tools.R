#' Convert video to image
#' @inheritParams proccess_video
#' @family video_tools
#' @export
convert_video_to_image <- function(video_path, frames_path, fps = 5) {

  if (fs::dir_exists(frames_path)) {
    fs::dir_delete(frames_path)
  }
  # av::av_log_level(set = "-8")
  av::av_video_images(
    video = video_path,
    destdir = frames_path,
    format = "jpg",
    fps = fps
  )
}

#' Convert video to image
#' @inheritParams proccess_video
#' @family video_tools
#' @export
extract_video_info <- function(video_path) {
  `%>%` <- dplyr::`%>%`

  video_info <- av::av_media_info(video_path)

  video_info$video$framerate <- as.character(
    round(as.numeric(video_info$video$framerate), 2)
  )

  tibble::tibble(
    var = colnames(video_info$video),
    value = unlist(video_info$video)
  ) %>%
    dplyr::bind_rows(
      tibble::tibble(var = "duration", value = as.character(
        round(video_info$duration, 2)
        )
      )
    )

}


#' Extract data from video
#'
#' @description Extract data and frames from video.
#' @param video_path Path to video file.
#' @param subject_model model generated from `generate_subject_model()`.
#' @param frames_path Path to save frames extracted from video.
#' @param coord1 length 2 vector with position of the top left point.
#' @param coord2 length 2 vector with position of the bottom right point.
#' @param fps default = 5; Frames per second to decompose video.
#' @family video_tools
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

  frames_vector <- convert_video_to_image(
    video_path = video_path, frames_path = frames_path, fps = fps
  )

  first_frame <- EBImage::readImage(frames_vector[1])


  if (isTRUE(is.null(coord1) & is.null(coord2))) {
    area_x_min <- 0
    area_x_max <- dim(first_frame@.Data)[1]
    area_y_min <- 0
    area_y_max <- dim(first_frame@.Data)[2]

  } else {
    area_x_min <- coord1[1]
    area_x_max <- coord2[1]
    area_y_min <- coord1[2]
    area_y_max <- coord2[2]
  }

  area_x_range <- area_x_min:area_x_max
  area_y_range <- area_y_min:area_y_max

  im2 <- first_frame

  maat <- first_frame@.Data[area_x_range, area_y_range, ]
  im2@.Data <- maat

  # Progress bar count
  prog_count <- progressr::progressor(along = frames_vector)

  # frame_path = frames_vector[2]
  extract_values <- function(frame_path) {
    `%>%` <- dplyr::`%>%`
    first_frame <- EBImage::readImage(frame_path)

    maat <- first_frame@.Data[area_x_range, area_y_range, ]

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
