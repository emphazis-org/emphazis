testthat::test_that("App starts", {
  testthat::expect_message(
    emphazis_app(
      host = "0.0.0.0",
      port = NULL,
      launch_browser = FALSE,
      test = TRUE
    ),
    regexp = "Running test"
  )
})
