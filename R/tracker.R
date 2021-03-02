#' Generate subject model
#'
#' Generate subject model.
#' @param subject_path Path to subject image.
#' @param background_path Path to background image.
#' @export
generate_subject_model <- function(subject_path, background_path) {
  bg_image <- EBImage::readImage(background_path)
  subject_image <- EBImage::readImage(subject_path)

  bg_mat <- cbind(
    c(bg_image@.Data[, , 1]),
    c(bg_image@.Data[, , 2]),
    c(bg_image@.Data[, , 3])
  )
  subject_mat <- cbind(
    c(subject_image@.Data[, , 1]),
    c(subject_image@.Data[, , 2]),
    c(subject_image@.Data[, , 3])
  )

  bg_mat <- bg_mat[sample(seq_len(nrow(bg_mat))), ]
  subject_mat <- subject_mat[sample(seq_len(nrow(subject_mat))), ]

  Mat <- rbind(cbind(bg_mat[1:500, ], 0), cbind(subject_mat[1:500, ], 1))
  colnames(Mat) <- c("R", "G", "B", "Y")

  subject_model <- base::suppressWarnings({
    stats::glm(
      formula = Y ~ R * G * B,
      data = base::data.frame(Mat),
      family = stats::binomial()
    )
  })
  return(subject_model)
}

#' Extract data from video
#'
#' Extract data and frames from video.
#' @param frames_path Path to save frames extracted from video.
#' @param subject_model model generated from `generate_subject_model()`.
#' @param coord1 length 2 vector with position of the top right point.
#' @param coord2 length 2 vector with position of the bottom left point.
#' @export
proccess_video <- function(
                           video_path, frames_path, subject_model,
                           coord1 = c(285, 655),
                           coord2 = c(475, 20)) {
  `%>%` <- dplyr::`%>%`
  if (fs::dir_exists(frames_path)) {
    fs::dir_delete(frames_path)
  }
  frames_vector <- av::av_video_images(
    video = video_path, destdir = frames_path, format = "jpg", fps = 3
  )
  # base_names <- list.files(frames_path)
  im <- EBImage::readImage(frames_vector[1])

  # TODO criar um slider no App para definir a area de corte da arena
  Coord1 <- coord1
  Coord2 <- coord2

  im2 <- im
  maat <- im@.Data[Coord1[1]:Coord1[2], Coord2[1]:Coord2[2], ]
  im2@.Data <- maat

  # frame_path = frames_vector[2]
  extract_values <- function(frame_path) {
    `%>%` <- dplyr::`%>%`
    im <- EBImage::readImage(frame_path)
    maat <- im@.Data[Coord1[1]:Coord1[2], Coord2[1]:Coord2[2], ]
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

    frame_hull_temp <- imM %>%
      base::as.data.frame() %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      tibble::rowid_to_column(var = "y") %>%
      tidyr::pivot_longer(cols = -c("y"), names_to = "x") %>%
      dplyr::mutate(x = as.integer(stringr::str_remove(x, "^V"))) %>%
      dplyr::filter(value != 0)

    area <- EBImage::computeFeatures.shape(imM)

    base::suppressWarnings({
      id <- area[, 1] == max(area[, 1])
    })


    x_y_center <- EBImage::computeFeatures.moment(imM)[id][1:2]

    Res_temp <- tibble::tibble(
      x_center = x_y_center[1],
      y_center = x_y_center[2]
    )

    temp_list <- list(
      frame_hull_temp,
      Res_temp
    )
    return(temp_list)
  }
  res_list <- purrr::map(
    frames_vector,
    extract_values
  )
  return(res_list)
}
