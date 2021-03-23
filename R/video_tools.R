#' Convert video to image
#' @inheritParams proccess_video
#' @family video_tools
#' @export
convert_video_to_image <- function(video_path, frames_path, fps = 5) {
  if (fs::dir_exists(frames_path)) {
    fs::dir_delete(frames_path)
  }

  invisible(capture.output(
    {
      video_images_vector <- av::av_video_images(
        video = video_path,
        destdir = frames_path,
        format = "jpg",
        fps = fps
      )
    },
    type = "message"
  ))
  # TODO add interactive check for verbose output
  # rlang::inform("Images from video extracted succesfully")
  return(video_images_vector)
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
      ))
    )
}

#' Extract data from video
#'
#' @description Extract data and frames from video.
#' @param video_path Path to video file.
#' @param subject_model Model generated from `generate_subject_model()`,
#' or path to folder with model files.
#' @param frames_path Path to save frames extracted from video.
#' @param coord1 Length 2 vector with position of the top left point.
#' @param coord2 Length 2 vector with position of the bottom right point.
#' @param fps default = 5; Frames per second to decompose video.
#' @param method Default
#'
#' @family video_tools
#'
#' @export
proccess_video <- function(
                           video_path,
                           subject_model,
                           frames_path,
                           coord1 = NULL,
                           coord2 = NULL,
                           fps = 5,
                           method = "glm") {
  `%>%` <- dplyr::`%>%`

  if (isTRUE(method == "glm")) {
    position_table <- proccess_video_glm(
      video_path,
      subject_model,
      frames_path,
      coord1 = NULL,
      coord2 = NULL,
      fps = 5
    )
  }

  if (isTRUE(method == "yolo")) {
    if (!requireNamespace("emphaziscv", quietly = FALSE)) {
      error_msg <- glue::glue_collapse(
        x = c(
          "`emphaziscv` package not installed.",
          "Check documentaion on how to install."
        ),
        sep = "\n"
      )
      rlang::abort(error_msg)
    }

    position_table <- emphaziscv::proccess_video_yolo(
      video_path = video_path,
      subject_model = subject_model,
      fps = fps
    )
  }

  emphazis_version <- as.character(utils::packageVersion("emphazis"))
  unit_used <- "px"
  analysis_date <- format(base::Sys.time(), "%y%m%d-%H%M%S-UTC", tz = "UTC")
  base::attr(x = position_table, "emphazis_version") <- emphazis_version
  base::attr(x = position_table, "unit") <- unit_used
  base::attr(x = position_table, "analysis_date") <- analysis_date

  # arena_width_unit <- max(area_x_range) - min(area_x_range)
  # base::attr(x = position_table, "arena_width") <- arena_width_unit
  # arena_height_unit <- max(area_y_range) - min(area_y_range)
  # base::attr(x = position_table, "arena_height") <- arena_height_unit

  # base::attr(x = position_table, "class") <- c(
  #   class(position_table), "emphazis_tbl"
  # )

  return(position_table)
}

#' @noRd
proccess_video_glm <- function(
  video_path,
  subject_model,
  frames_path,
  coord1 = NULL,
  coord2 = NULL,
  fps = 5,
  envir = parent.frame()
) {
  video_image_vector <- convert_video_to_image(
    video_path = video_path,
    frames_path = frames_path,
    fps = fps
  )

  # Progress bar count
  prog_count <- progressr::progressor(
    along = c(video_image_vector, 1,2),
    envir = envir
  )
  prog_count()
  first_frame <- EBImage::readImage(video_image_vector[1])

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

  prog_count()

  max_width <- av::av_media_info(video_image_vector[1])$video$width
  max_height <- av::av_media_info(video_image_vector[1])$video$height
  if (isTRUE(area_x_max >= max_width)) {
    area_x_max <- max_width
  }
  if (isTRUE(area_y_max >= max_height)) {
    area_y_max <- max_height
  }
  if (isTRUE(area_x_max < area_x_min)) {
    rlang::abort(
      "X value from coordinates 2, need to be greater than coordinates 1."
    )
  }
  if (isTRUE(area_y_max < area_y_min)) {
    rlang::abort(
      "Y value from coordinates 2, need to be greater than coordinates 1."
    )
  }

  area_x_range <- area_x_min:area_x_max
  area_y_range <- area_y_min:area_y_max

  im2 <- first_frame

  maat <- first_frame@.Data[area_x_range, area_y_range, ]
  im2@.Data <- maat


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

  position_table <- purrr::map_dfr(
    video_image_vector,
    extract_values
  )

  arena_width_unit <- max(area_x_range) - min(area_x_range)
  base::attr(x = position_table, "arena_width") <- arena_width_unit
  arena_height_unit <- max(area_y_range) - min(area_y_range)
  base::attr(x = position_table, "arena_height") <- arena_height_unit
  base::attr(x = position_table, "fps") <- fps

  return(position_table)
}
