# if (!(base::requireNamespace(emphazis))) {
#  install.packages("emphazis"); require(emphazis, quietly = TRUE)
#}

pkg_deps <- c(
  "shiny",
  "bslib",
  "thematic",
  "shinycssloaders",
  "colourpicker",
  "lubridate",
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

  theme = bslib::bs_theme(bootswatch = "flatly", version = 4),

  ### WELCOME PAGE ----
  shiny::tabPanel(
    title = "Welcome",

    # HTML meta tags
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
          shiny::tags$h1(
            shiny::tags$p("Empha",shiny::tags$strong("Z"), "is"),
            align = "center"
          ),

          shiny::tags$br(),
          shiny::tags$div(
            shiny::tags$img(src = "logo-emphazis.png"),
            style = "text-align: center;"
          ),
          shiny::tags$br(),
          shiny::tags$hr(),
          shiny::tags$br(),

          shiny::tags$h2("Workflow"),

          shiny::tags$br(),
          shiny::tags$p(
            ""
          ),
          shiny::tags$br(),
          shiny::tags$hr(),

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
          shiny::tags$p("Grant number: 18/07098-0"),

          shiny::tags$br(),
          shiny::tags$hr(),

          shiny::tags$h2("Information"),
          shiny::tags$p(
            "You are currently running emphazis version: ",
            shiny::tags$strong(
              as.character(utils::packageVersion("emphazis"))
            )
          )
        )
      )
    )
  ),

  # Upload panel ------------------------------------------------
  shiny::tabPanel(
    title = "Analysis",
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Upload files",
        shiny::sidebarLayout(
          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            shiny::tags$h3(
              shiny::tags$em(
                shiny::tags$strong(
                  "Upload video"
                )
              )
            ),

            shiny::tags$br(),
            # Video Input
            shiny::fileInput(
              inputId = "input_video",
              label = "Choose Video File",
              multiple = FALSE,
              accept = c("video/*")
            ),
            # Horizontal line ----
            shiny::tags$hr(),
            # Select method
            shiny::selectInput(
              inputId = "analysis_method",
              label = shiny::tags$h3(
                shiny::tags$em("Select Analysis Method")
              ),
              choices = c(GLM = "glm", YOLO = "yolo"),
              selected = "glm"
            ),
            shiny::tags$hr(),

            # # GLM options
            # shiny::conditionalPanel(
            #   condition = "input.analysis_method == 'glm'",
            #   shiny::selectInput(
            #     "breaks", "Breaks",
            #     c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
            #   ),
            #
            #   # Only show this panel if yolo is selected
            #   shiny::conditionalPanel(
            #     condition = "input.analysis_method == 'yolo'",
            #     shiny::sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
            #   )
            # ),

            shiny::sliderInput(
              inputId = "fps_slider",
              min = 1, max = 24, step = 1,
              round = TRUE, value = 5,
              label = "Choose Frames per Second"
            ),

            shiny::actionButton(
              inputId = "run_video_process",
              label = "Decompose video",
              icon = shiny::icon("film")
            ),

            shiny::tags$div(
              shiny::tags$b("Video details"),
              shinycssloaders::withSpinner(
                shiny::tableOutput(
                  outputId = "video_description"
                )
              )
            )
          ),

          # Main panel for image inputs
          mainPanel = shiny::mainPanel(
            shiny::tags$div(
              shiny::tags$b("Video decomposed frame"),
              shiny::tags$br(),
              shinycssloaders::withSpinner(
                shiny::imageOutput(
                  outputId = "input_first_frame"
                )
              )
            )
          )
        )
      ),
      # Arena selection panel ------------------------------------------------
      shiny::tabPanel(
        title = "Arena settings",
        shiny::sidebarLayout(
          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            shiny::tags$h3(
              shiny::tags$em(
                shiny::tags$strong(
                  "Arena selection"
                )
              )
            ),
            shiny::tags$hr(),
            # Copy the line below to make a set of radio buttons
            shiny::radioButtons(
              inputId = "arena_coord_radio",
              label = shiny::tags$h3("Coord to select"),
              choices = list("Top left" = 1, "Bottom right" = 2, "Area" = 3),
              selected = 3
            ),
            shinycssloaders::withSpinner(
              shiny::tableOutput(
                "arena_coord_info"
              )
            ),
            shiny::actionButton(
              "cut_arena_button","Apply slice"
            ),

            shiny::actionButton(
              "restart_arena_button", "Restart"
            ),
            shiny::tags$hr(),
            shiny::tags$br()
          ),

          # Main panel for image inputs
          mainPanel = shiny::mainPanel(

            shiny::tags$div(
              shiny::tags$b("Selected arena"),
              shiny::imageOutput(
                outputId = "input_cut_frame",
                click = "arena_click",
                brush = "arena_brush"
              )
            ),

            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),


            shiny::tags$div(
              shiny::tags$b("Sliced arena"),
              shinycssloaders::withSpinner(
                shiny::imageOutput(
                  outputId = "sliced_arena"
                )
              )
            )
          )
        )
      ),

      # Subject selection inner tab ---------------------------------------------
      shiny::tabPanel(
        title = "Subject settings",
        shiny::sidebarLayout(
          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            shiny::tags$h3(
              shiny::tags$em(
                shiny::tags$strong(
                  "Subject selection"
                )
              )
            ),
            shiny::tags$hr(),
            # Copy the line below to make a set of radio buttons
            shiny::radioButtons(
              inputId = "subject_coord_radio",
              label = shiny::tags$h3("Coord to select"),
              choices = list("Top left" = 1, "Bottom right" = 2, "Area" = 3),
              selected = 3
            ),
            shinycssloaders::withSpinner(
              shiny::tableOutput(
                "subject_coord_info"
              )
            ),
            shiny::actionButton(
              "cut_subject_button","Apply slice"
            ),

            shiny::actionButton(
              "restart_subject_button", "Restart"
            )
          ),
          # Main panel for image inputs
          mainPanel = shiny::mainPanel(
            shiny::tags$div(
              shiny::tags$b("Selected subject"),
              shinycssloaders::withSpinner(
                shiny::imageOutput(
                  outputId = "subject_select",
                  click = "subject_click",
                  brush = "subject_brush"
                )
              )
            ),

            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$br(),

            shiny::tags$div(
              shiny::tags$b("Sliced subject"),
              shinycssloaders::withSpinner(
                shiny::imageOutput(
                  outputId = "sliced_subject"
                )
              )
            )
          )
        )
      ),
      # Run Analysis inner tab ------------------------------------------
      shiny::tabPanel(
        title = "Run analysis",
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
            shiny::actionButton(
              inputId = "start_job_button",
              label = shiny::tags$em(
                shiny::tags$b(
                  "Click to start analysis!"
                )
              ),
              icon = shiny::icon("rocket")
            )
          ),
          mainPanel = shiny::mainPanel(
            shiny::textOutput("time_passed")
          )
        )
      )
    )
  ),
  # Analysis Panel ------------------------------------------------------
  shiny::tabPanel(
    title = "Summary statistics",
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 3,

        # shiny::tags$h3(
        #   shiny::tags$em(
        #     shiny::tags$strong(
        #       "Color palette segmentation"
        #     )
        #   )
        # ),
        # shiny::tags$br(),

        shiny::tags$h3(
          shiny::tags$em(
            shiny::tags$strong(
              "Set unit parameters"
            )
          )
        ),
        shiny::tags$br(),
        shiny::radioButtons(
          inputId = "conversion_unit_radio",
          label = shiny::tags$h4("Unit to convert"),
          choices = list(
            "Centimeter (cm)" = 1,
            "Inch (in)" = 2,
            "Pixel (px)" = 3
          ),
          selected = 3
        ),
        shiny::numericInput(
          inputId = "arena_width",
          label = shiny::tags$em("Arena width (unit)"),
          value = 1,
          min = 0.1,
          max = NA,
          step = 0.1,
          width = "50%"
        ),

        shiny::numericInput(
          inputId = "arena_height",
          label = shiny::tags$em("Arena height (unit)"),
          value = 1,
          min = 0.1,
          max = NA,
          step = 0.1,
          width = "50%"
        ),
        shiny::numericInput(
          inputId = "conversion_rate_width",
          label = shiny::tags$h5("Height conversion rate (Pixel to Unit)"),
          value = 1.000,
          min = 0.010,
          max = NA,
          step = 0.001,
          width = "50%"
        ),

        shiny::numericInput(
          inputId = "conversion_rate_height",
          label = shiny::tags$h5("Height conversion rate (Pixel to Unit)"),
          value = 1.000,
          min = 0.010,
          max = NA,
          step = 0.001,
          width = "50%"
        ),


        shiny::tags$br(),
        # Button to update statistics
        shiny::actionButton(
          inputId = "update_summary_button",
          label = "Update data!",
          icon = shiny::icon("table")
        ),
        shiny::tags$hr()
      ),

      mainPanel = shiny::mainPanel(
        shiny::tags$b("Summary table"),
        # shiny::tags$br(),
        # shinyWidgets::progressBar(
        #   id = "analysis_prog_bar", value = 0, total = 100
        # ),
        shiny::tags$br(),
        shinycssloaders::withSpinner(
          shiny::tableOutput("analysis_summary")
        )
      )
    )
  ),

  # Plots panel set -----------------------------------------------------------
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
          min = 1, max = 100, value = c(1, 100)
        ),
        shiny::tags$br(),
        # Button to start
        shiny::actionButton(
          inputId = "update_plots",
          label = shiny::tags$em("Update Plots"),
          icon = shiny::icon("chart-bar")
        ),
        shiny::tags$hr(),
        shiny::tags$br(),
        colourpicker::colourInput(
          inputId = "color_subject_1",
          label = shiny::tags$h3("Select subject color"),
          value = "purple"
        ),
        shiny::tags$br()
      ),
      mainPanel = shiny::mainPanel(
        shiny::tabsetPanel(
          # 2D plots tab --------
          shiny::tabPanel(
            title = shiny::tags$h5(
              shiny::tags$strong("2D-Plots")
            ),
            shiny::plotOutput(
              "plot_track"
            ) %>%
              shiny::tagAppendAttributes(
                alt = "Tracking Plot of the arena"
              ) %>%
              shinycssloaders::withSpinner(),
            shiny::plotOutput(
              "plot_track_heatmap"
            ) %>%
              shiny::tagAppendAttributes(
                alt = "Heatmap plot of the arena"
              ) %>%
              shinycssloaders::withSpinner(),
            shiny::plotOutput(
              "plot_dist"
            ) %>%
              shiny::tagAppendAttributes(
                alt = "Plot of cumulative distance"
              ) %>%
              shinycssloaders::withSpinner(),
            shiny::plotOutput(
              "plot_speed"
            ) %>%
              shiny::tagAppendAttributes(
                alt = "Plot of rolling average speed by time"
              ) %>%
              shinycssloaders::withSpinner()
          ),

          shiny::tabPanel(
            title = shiny::tags$h5(
              shiny::tags$strong("3D-Plots")
            ),
            plotly::plotlyOutput(
              "plot_3d_dots"
            ) %>%
              shinycssloaders::withSpinner(),
            plotly::plotlyOutput(
              "plot_3d_surface"
            ) %>%
              shinycssloaders::withSpinner(),
            plotly::plotlyOutput(
              "plot_3d_lines"
            ) %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  ),

#  # 3D plots panel ------------------------------------------------------
#  shiny::tabPanel(
#    title = "3D-Plots",
#    shiny::sidebarLayout(
#      sidebarPanel = shiny::sidebarPanel(
#        width = 3,
#        shiny::tags$h3(
#          shiny::tags$em(
#            shiny::tags$strong(
#              "Plot parameters"
#            )
#          )
#        ),
#        shiny::tags$br()
#      ),
#      mainPanel = shiny::mainPanel(
#      )
#    )
#  ),
#
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
