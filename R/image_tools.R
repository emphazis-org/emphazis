#' Slice image
#'
#' @description Slice image with given X and Y coordinates.
#'
#' @param image_path Path to image file.
#' @param output_path Path to save sliced image.
#' @inheritParams proccess_video
#' @family image_tools
#' @export
slice_image <- function(
                        image_path,
                        output_path,
                        coord1 = NULL,
                        coord2 = NULL) {
  if (!fs::dir_exists(output_path)) {
    fs::dir_create(output_path)
  }

  image_obj <- EBImage::readImage(image_path)

  image_data <- image_obj@.Data
  # dim(image_data)
  if (isTRUE(is.null(coord1) & is.null(coord2))) {
    area_x_min <- 0
    area_x_max <- dim(image_data)[1]
    area_y_min <- 0
    area_y_max <- dim(image_data)[2]
  } else {
    area_x_min <- coord1[1]
    area_x_max <- coord2[1]
    area_y_min <- coord1[2]
    area_y_max <- coord2[2]
  }

  area_x_range <- area_x_min:area_x_max
  area_y_range <- area_y_min:area_y_max

  image_obj@.Data <- image_data[area_x_range, area_y_range, ]


  savefile_path <- fs::path(
    output_path,
    stringr::str_replace(basename(image_path), ".jpg", "_slice.jpg")
  )

  EBImage::writeImage(image_obj, files = savefile_path)

  return(savefile_path)
}

#' Convert image sizes
#'
#' @description Convert image sizes to pixel from other units.
#'
#' @param image_path Path to image.
#' @param width Image width in centimeter.
#' @param height Image height in centimeter.
#' @param dpi Dots Per Inch unit used for conversion.
#' @export
convert_image_size_unit <- function(
                                    image_path,
                                    width = NULL,
                                    height = NULL,
                                    dpi = NULL) {
  if (isTRUE(is.null(width) & is.null(height) & is.null(dpi))) {
    rlang::abort(
      "Width and height need to be supplied.\nOptionally, a dpi value can be supplied instead."
    )
  }

  if (isTRUE(!is.null(width) & !is.null(height))) {
    rlang::abort(
      "You need to supply only `height` or `width` for each call of `convert_image_size_unit()`."
    )
  }

  if (is.null(height)) {
    image_width_px <- av::av_media_info(image_path)$video$width
    width_conversion_value <- width / image_width_px
    conversion_rate <- width_conversion_value
  }
  if (is.null(width)) {
    image_height_px <- av::av_media_info(image_path)$video$height
    height_conversion_value <- height / image_height_px
    conversion_rate <- height_conversion_value
  }
  if (isTRUE(is.null(width) & is.null(height))) {
    conversion_rate <- 1 / dpi
  }

  return(conversion_rate)
}
