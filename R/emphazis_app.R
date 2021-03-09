#' Shiny interface for emphazis package
#'
#' @description `emphazis_app` Automated tracking analysis for emphazis package.
#'
#' @param port is the TCP port that the application should listen on. If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used.
#' Otherwise, use a random port.
#'
#' @param launch.browser If true, the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions only. This value of
#' this parameter can also be a function to call with the application's URL.
#'
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#'
#' @examples
#'
#' # emphazis_app()
#' @export
emphazis_app <- function(
                         host = "127.0.0.1", port = NULL, launch_browser = TRUE, test = FALSE) {
  if (isTRUE(test)) {
    return(message("Running test."))
  }
  shiny::runApp(
    appDir = system.file("emphazis_app", package = "emphazis"),
    launch.browser = launch_browser,
    port = port,
    host = base::getOption("shiny.host", host)
  )
}
