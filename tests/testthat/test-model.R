testthat::test_that("Model", {
  subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
  background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
  model_test <- generate_subject_model(subject_path, background_path)

  testthat::expect_s3_class(model_test, "glm")

  testthat::expect_equal(model_test$family$family, "binomial")
})
