# Define server logic ----
server <- function(
  input,
  output,
  session
) {

  ## stop the R session
  session$onSessionEnded(stopApp)
  ##
  ## To increase file upload max size
  # options(shiny.maxRequestSize = 900*1024^2) 90MB?
  base::options(shiny.maxRequestSize = 2800 * 1024 ^ 2) # 350MB?


  ### initial values, needed for reactivity ####
  react_values <- shiny::reactiveValues()

  # TODO remove path_table, it is not used
  react_values$subject_model <- NULL
  react_values$frames_output <- NULL
  react_values$path_table <- NULL
  react_values$dist_table <- NULL

  # react_values$slider_min <- 0
  # react_values$slider_max <- 10

  # Update Interface based on inputs ------------------------------------------

  shiny::observeEvent(input$update_slider, {
    if (!is.null(react_values$dist_table)) {
      max_slider_value <- length(
        unique(dplyr::pull(react_values$dist_table, "frame"))
      )
      shiny::updateSliderInput(
        inputId = "frame_range",
        max = max_slider_value,
        value = c(1, max_slider_value)
      )
    }
  })



  # Upload files pane --------------------------------------------------------
  # Print input images

  output$video_description <- shiny::renderText({
    shiny::req(input$input_video)

    file <- input$input_video
    file_path <- file$datapath
    file_path
  })

  output$subject <- shiny::renderImage({
    shiny::req(input$input_subject)

    file <- input$input_subject
    file_path <- file$datapath

    list(
      src = file_path,
      contentType = "image/*",
      width = 100,
      height = 100,
      alt = "Subject image"
    )
  }, deleteFile = FALSE
  )

  output$background <- shiny::renderImage({
    shiny::req(input$input_bg)

    file <- input$input_bg
    file_path <- file$datapath

    list(
      src = file_path,
      contentType = "image/*",
      with = 300,
      height = 300,
      alt = "Background image"
    )
  },  deleteFile = FALSE
  )


  # Analysis pane -------------------------------------------------------------

  shiny::observeEvent(input$start_job, {

    shiny::req(
      input$input_video,
      input$input_subject,
      input$input_bg
    )

    message("Start analysis")

    fs::file_exists(input$input_subject$datapath)
    fs::file_exists(input$input_bg$datapath)
    fs::file_exists(input$input_video$datapath)


    coord_1 <- c(
      input$arena_x_1,
      input$arena_y_1
    )

    coord_2 <- c(
      input$arena_x_2,
      input$arena_y_2
    )
    react_values$subject_model <- emphazis::generate_subject_model(
      subject_path = input$input_subject$datapath,
      background_path = input$input_bg$datapath
    )

    video_path <- input$input_video$datapath

    temp_frames_path <- fs::path_temp("frames")
    react_values$frames_output <- emphazis::proccess_video(
      video_path = video_path,
      frames_path = temp_frames_path,
      subject_model = react_values$subject_model,
      coord1 = coord_1,
      coord2 = coord_2
    )

    message("Mid analysis")

    react_values$dist_table <- emphazis::calculate_distances(react_values$frames_output)
    react_values$path_table <- emphazis::prepare_path_data(react_values$frames_output)

    message("Finished analysis")

  })

  output$analysis_summary <- shiny::renderTable({

    shiny::req(react_values$dist_table)

    head(react_values$dist_table)

  })

  # Plots pane -----------------------------------------------------------------
  output$plot_track <- shiny::renderPlot({

    shiny::req(
      react_values$path_table,
      react_values$dist_table
    )

    emphazis::plot_track(
      path_table = react_values$path_table,
      dist_table = react_values$dist_table,
      color = input$color_subject_1,
      range = input$frame_range
    )
  })

  output$plot_track_heatmap <- shiny::renderPlot({

    shiny::req(
      react_values$path_table,
      react_values$dist_table
    )

    emphazis::plot_track_heatmap(
      path_table = react_values$path_table,
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

  output$plot_dist <- shiny::renderPlot({

    shiny::req(react_values$dist_table)

    emphazis::plot_cumulative_distance(
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

  output$plot_speed <- shiny::renderPlot({

    shiny::req(react_values$dist_table)

    emphazis::plot_average_speed(
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

  # 3d Plots pane --------------------------------------------------------------
  output$plot_3d_dots <- plotly::renderPlotly({
    shiny::req(react_values$dist_table)

    emphazis::plot_3d_dots(
      dist_table = react_values$dist_table,
      size = 3
    )

  })

  output$plot_3d_lines <- plotly::renderPlotly({
    shiny::req(react_values$dist_table)

    emphazis::plot_3d_lines(dist_table = react_values$dist_table)

  })

  output$plot_3d_surface <- plotly::renderPlotly({
    shiny::req(react_values$dist_table)

    emphazis::plot_3d_surface(dist_table = react_values$dist_table)

  })

}
