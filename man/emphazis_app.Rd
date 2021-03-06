% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emphazis_app.R
\name{emphazis_app}
\alias{emphazis_app}
\title{Shiny interface for emphazis package}
\usage{
emphazis_app(
  host = "127.0.0.1",
  port = NULL,
  launch_browser = TRUE,
  test = FALSE
)
}
\arguments{
\item{host}{The IPv4 address that the application should listen on.
Defaults to the shiny.host option, if set, or "127.0.0.1" if not.}

\item{port}{is the TCP port that the application should listen on. If the port is not specified,
and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used.
Otherwise, use a random port.}

\item{launch.browser}{If true, the system's default web browser will be launched automatically
after the app is started. Defaults to true in interactive sessions only. This value of
this parameter can also be a function to call with the application's URL.}
}
\description{
\code{emphazis_app} Automated tracking analysis for emphazis package.
}
\examples{

# emphazis_app()
}
