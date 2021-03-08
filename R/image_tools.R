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
  coord2 = NULL
) {

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
#' @inheritParams proccess_video
#' @export
convert_image_size_unit <- function(
  image_path,
  width = NULL,
  height = NULL,
  dpi = NULL,
  unit = "cm"
) {
  if (isTRUE(is.null(width) & is.null(height) & is.null(dpi))) {
    stop(
      "width and height need to be supplied.\n",
      "Optionally, a dpi value can be supplied instead."
    )
  }
  # TODO implement conversion through dpi value
  image_width_px <- av::av_media_info(image_path)$video$width
  image_height_px <- av::av_media_info(image_path)$video$height

  width_conversion_value <- width/image_width_px
  height_conversion_value <- height/image_height_px

  return(c(width_conversion_value, height_conversion_value))
}
