testthat::test_that("App starts", {
  testthat::expect_message(
    emphazis_app(launch.browser = FALSE, test = TRUE),
    regexp = "Running test"
  )
})
