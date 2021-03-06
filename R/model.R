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

  Mat <- rbind(cbind(bg_mat[ , ], 0), cbind(subject_mat[ , ], 1))
  #Mat <- rbind(cbind(bg_mat[1:500, ], 0), cbind(subject_mat[1:500, ], 1))
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
