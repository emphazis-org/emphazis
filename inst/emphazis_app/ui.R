# if (!(require(emphazis))) {
#  install.packages("emphazis"); require(emphazis, quietly=TRUE)
#}

pkg_deps <- c(
  "shiny",
  "shinycssloaders",
  "shinythemes",
  "metathis"
)

for (pkg_name in pkg_deps) {
 if (!(base::requireNamespace(pkg_name, quietly = TRUE))) {
    utils::install.packages(
      pkg_name,
      repos = "https://packagemanager.rstudio.com/all/latest"
    )
 }
}

# TODO remove shiny invocation
require(shiny, quietly = TRUE)

`%>%` <- dplyr::`%>%`

# Main NavBar ----
# TODO replace it with `withr::local_option`
base::options(spinner.size = 1, spinner.type = 5)

ui <-  shiny::navbarPage(
  title = "EmphaZis",
  theme = shinythemes::shinytheme("flatly"),

  ### WELCOME PAGE ----
  shiny::tabPanel(
    title = "Welcome",

    shiny::tags$head(
      # HTML <meta> tag
      metathis::meta() %>%
        metathis::meta_social(
          title = "EmphaZis",
          description = "Zebrafish tracking made simple",
          url = "https://www.emphazis.org",
          image = "https://raw.githubusercontent.com/emphazis-org/emphazis/main/inst/emphazis_app/www/logo-emphazis.png",
          image_alt = "EmphaZis"
        ),

      ## TODO prepare GA_TOKEN to be added to HTML file only when not on CI/CD
      # Google Analytics
      {
        if (isTRUE(Sys.getenv("GA_TOKEN") != "")) {
          ga_file_path <- "google-analytics.html"
          if (fs::file_exists(ga_file_path)) {
            shiny::includeHTML(ga_file_path)
          }
        }
      },

      # Add website favicon
      shiny::tags$link(rel = "icon", href = "icons/logo.png"),

      # HTML meta tags


      # JS code for ggplot plot re-size
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
          shiny::tags$h1("EmphaZis", align = "center"),
          # shiny::tags$div(
          #   shiny::tags$h1("EmphaZis: tracking movement ...", align = "center")
          # ),
          # shiny::tags$br(),
          # shiny::tags$h4(
          #   shiny::tags$em(
          #     shiny::tags$strong(
          #       "Effects of marine pharmaceuticals in Zebrafish and ZFL cell line.",
          #       align = "center"
          #     )
          #   )
          # ),

          shiny::tags$br(),
          shiny::tags$div(
            shiny::tags$img(src = "logo-emphazis.png"),
            style = "text-align: center;"
          ),
          shiny::tags$br(),
          shiny::tags$h2("Motivation"),

          shiny::tags$br(),
          shiny::tags$p(
            "This web application was developed to meet the needs of the FAPESP ", shiny::tags$em("Emphasis"), " project, which aims to evaluate the effects of pharmaceuticals of marine origin in Zebrafish and ZFL cell line."
          ),
          shiny::tags$br(),
          shiny::tags$p(
            "Multidisciplinarity in Science. Team work."
          ),
          shiny::tags$br(),
          shiny::tags$p(
            "Science advances when it embraces different areas in a common goal."
          ),
          shiny::tags$br(),

          shiny::tags$p("This work is supported by FAPESP"),
          shiny::tags$p("Grant number: 18/07098-0")
        )
      )
    )
  ),

  # Upload panel ------------------------------------------------
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
        shiny::textOutput("video_description"),
        shiny::tags$div(
          shiny::imageOutput("subject"),
          shiny::imageOutput("background")
        )
      )
    )
  ),

  # Analysis Panel -------------------------------------------------------
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
        shiny::tags$hr()
      ),
      mainPanel = shiny::mainPanel(
        shiny::dataTableOutput("analysis_summary")
      )
    )
  ),

  # 2D plots panel ------------------------------------------------------
  shiny::tabPanel(
    title = "Plots",
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 3,
        shiny::tags$h3(
          shiny::tags$em(
            shiny::tags$strong(
              "Plot parameters"
            )
          )
        ),
        shiny::tags$br(),
        shiny::sliderInput(
          inputId = "frame_range",
          label = shiny::tags$h3("Choose Frame:"),
          min = 1, max = 10, value = c(1, 10)
        ),
        shiny::tags$br(),
        # Button to start
        shiny::actionButton(
          "update_slider", "Update slider"
        ),
        shiny::tags$hr()
      ),
      mainPanel = shiny::mainPanel(
        shiny::plotOutput(
          "plot_track"
        ),
        shiny::plotOutput(
          "plot_track_heatmap"
        ),
        shiny::plotOutput(
          "plot_dist"
        ),
        shiny::plotOutput(
          "plot_speed"
        )
      )
    )
  ),

  # 3D plots panel ------------------------------------------------------
  shiny::tabPanel(
    title = "3D-Plots",
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 3,
        shiny::tags$h3(
          shiny::tags$em(
            shiny::tags$strong(
              "Plot parameters"
            )
          )
        ),
        shiny::tags$br()
      ),
      mainPanel = shiny::mainPanel(
        plotly::plotlyOutput(
          "plot_3d_dots"
        ),
        plotly::plotlyOutput(
          "plot_3d_surface"
        ),
        plotly::plotlyOutput(
          "plot_3d_lines"
        )
      )
    )
  ),

  # Quit button ---------------------------------------------------------
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
