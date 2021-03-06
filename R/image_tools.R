#' Slice image
#'
#' @description Slice image with given X and Y coordinates.
#'
#' @param image_path Path to image file.
#' @param output_path Path to save sliced image.
#' @inheritParams proccess_video
#' @family image_tools
#' @export
slice_image <- function(image_path, output_path, coord1 = NULL, coord2 = NULL) {

  if (!fs::dir_exists(output_path)) {
    fs::dir_create(output_path)
  }

  image_obj <- EBImage::readImage(image_path)

  image_data <- image_obj@.Data

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
