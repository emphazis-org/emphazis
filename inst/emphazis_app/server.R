# Define server logic ----
server <- function(input, output, session) {

  ## stop the R session
  session$onSessionEnded(stopApp)
  ##
  ## To increase file upload max size
  base::options(shiny.maxRequestSize = 200*1024^2)

  ### initial values, needed for reactivity ####
  react_values <- shiny::reactiveValues()

  # Upload files pane ---------------------------------------------
  # Print input images
  output$subject <- shiny::renderImage({

    file <- input$input_subject
    file_path <- file$datapath

    list(
      src = file_path,
      contentType = "image/*",
      width = 100,
      height = 100,
      alt = "Subject image"
    )
  })
  output$background <- shiny::renderImage({

    file <- input$input_bg
    file_path <- file$datapath

    list(
      src = file_path,
      contentType = "image/*",
      with = 300,
      height = 300,
      alt = "Background image"
    )
  })
  # Analysis pane ---------------------------------------------

  shiny::observeEvent(input$start_job, {
    react_values$subject_model <- emphazis::generate_subject_model(
      subject_path = fs::path_package("emphazis", "extdata", "subject.jpg"),
      background_path = fs::path_package("emphazis", "extdata", "background.jpg")
    )
    video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
    temp_frames_path <- fs::path_temp("frames")
    react_values$frames_output <- emphazis::proccess_video(
      video_path = video_path,
      frames_path = temp_frames_path,
      subject_model = react_values$subject_model,
      coord1 = c(285, 655),
      coord2 = c(475, 20)
    )

    react_values$dist_table <- emphazis::calculate_distances(react_values$frames_output)
    react_values$path_table <- emphazis::prepare_path_data(react_values$frames_output)
  })

  # Plots pane ------------------------------------------------
  output$plot_track <- shiny::renderPlot({
    emphazis::plot_track(
      path_table = react_values$path_table,
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

  output$plot_dist <- shiny::renderPlot({
    emphazis::plot_cumulative_distance(
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

  output$plot_speed <- shiny::renderPlot({
    emphazis::plot_average_speed(
      dist_table = react_values$dist_table,
      range = input$frame_range
    )
  })

}
