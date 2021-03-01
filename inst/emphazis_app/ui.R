# if (!(require(emphazis))) {
#  install.packages("emphazis"); require(emphazis, quietly=TRUE)
#}

pkg_deps <- c("shiny", "shinycssloaders", "shinythemes")
for (pkg_name in pkg_deps) {
  if (!(base::requireNamespace(pkg_name, quietly = TRUE))) {
    utils::install.packages(pkg_name, repos = "https://packagemanager.rstudio.com/all/latest")
  }
}

# TODO remove shiny invocation
require(shiny, quietly = TRUE)
# if (base::requireNamespace(package_name, quietly = TRUE))

# Main NavBar ----
# TODO replace it with `withr::local_option`
base::options(spinner.size = 1, spinner.type = 5)

ui <-  shiny::navbarPage(
  title = "(EMPHAZIS)",
  theme = shinythemes::shinytheme("flatly"),

  ### WELCOME PAGE ----
  shiny::tabPanel("Welcome",
    # JS code for ggplot plot resize
    shiny::tags$head(
      shiny::tags$script(
        '$(document).on("shiny:connected", function(e) {
        Shiny.onInputChange("innerWidth", window.innerWidth);
        });
        $(window).resize(function(e) {
        Shiny.onInputChange("innerWidth", window.innerWidth);
        });'
      )
    ),
    # end of code

    shiny::fluidRow(
      shiny::column(
        9,
        shiny::wellPanel(
          shiny::tags$h1("EMPHAZIS: tracking movement ...", align = "center"),
          shiny::tags$br(),
          shiny::tags$h4("EMPHAZIS description"),
          shiny::tags$br() #,
          # TODO add logo here
          # div()
        )
      )
    )
  ),
  shiny::tabPanel(
    title = "Upload files",
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 3,
        shiny::tags$h3(
          shiny::tags$em(
            shiny::tags$strong(
              "Upload files"
            )
          )
        ),
        shiny::tags$br(),
        # Video Input
        shiny::fileInput(
          "input_video", "Choose Video File",
          multiple = FALSE,
          accept = c("video/*")
        ),
        # Horizontal line ----
        shiny::tags$hr(),
        # Fish image input
        shiny::fileInput(
          "input_subject", "Choose Subject Image",
          multiple = FALSE,
          accept = c("image/*")
        ),
        # Horizontal line ----
        shiny::tags$hr(),
        # Background image input
        shiny::fileInput(
          "input_bg", "Choose Background Image",
          multiple = FALSE,
          accept = c("image/*")
        )
      ),

      # Main panel for image inputs
      mainPanel = shiny::mainPanel(
        shiny::imageOutput("subject"),
        shiny::imageOutput("background")
      )
    )
  ),
  shiny::tabPanel(
    title = "Analysis",
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 3,
        shiny::tags$h3(
          shiny::tags$em(
            shiny::tags$strong(
              "Start analysis"
            )
          )
        ),
        shiny::tags$br(),
        # Button to start
        shiny::actionButton(
          "start_job", "Click to start!"
        ),
        shiny::tags$hr(),
        # Slider to change frame
        shiny::sliderInput(
          inputId = "frame_range",
          label = shiny::tags$h3("Choose Frame:"),
          min = 1, max = 270, value = c(0,270)
        )
      ),
      mainPanel = shiny::mainPanel(
        shiny::plotOutput(
          "plot_track"
        ),
        shiny::plotOutput(
          "plot_dist"
        ),
        shiny::plotOutput(
          "plot_speed"
        )
      )
    ),
  ),
  ### Quit button ----
  shiny::navbarMenu(
    "Quit",
    shiny::tabPanel(
      shiny::actionLink(
        "stop_radiant",
        "Stop",
        icon = shiny::icon("power-off"),
        onclick = "setTimeout(function(){window.close();}, 100);"
      )
    )
  )
)
