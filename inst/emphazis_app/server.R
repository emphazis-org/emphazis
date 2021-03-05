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
  # options(shiny.maxRequestSize = 900*1024^2) 900MB?
  base::options(shiny.maxRequestSize = 900 * 1024 ^ 2)


  ### initial values, needed for reactivity ####
  react_values <- shiny::reactiveValues()

  react_values$subject_model <- NULL
  react_values$frames_output <- NULL
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

    # TODO use av::av_media_info to extract info
    video_details <- glue::glue(
      "File type: {file$type}; File size: {(file$size / (1024^2))} MB;"
    )
    video_details
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
      width = 300,
      height = 300,
      alt = "Background image"
    )
  },  deleteFile = FALSE
  )

  # Selection panel -----------------------------------------------------------

  observeEvent(input$run_video_process, {

    shiny::req(input$input_video)

    file.remove(
      paste(tempdir(), list.files(tempdir(), pattern = ".jpg"), sep = "/")
    )

    video_path <- input$input_video$datapath

    # Upload video
    emphazis::video_to_image(
      video_path = video_path, fps = input$fps_slider
    )
    frames <- list.files(tempdir(), pattern = ".jpg")

    video_time <- av::av_media_info(video_path)$duration[1]

    # progressr::withProgressShiny()


    ######################### show Frame
    output$input_first_frame <- shiny::renderImage({
      shiny::req(input$input_video)

      suppressWarnings(dir.create(tempdir()))
      # A temp file to save the output.
      # This file will be removed later by renderImage
      file_names <- list.files(path = tempdir())

      file_path <- paste(
        tempdir(), file_names[round(length(file_names) / 2, 0)], sep = "\\"
      )

      list(
        src = file_path,
        contentType = "image/*",
        width = 400,
        # height = 400,
        alt = "Image"
      )
    }, deleteFile = FALSE
    )
  })


  # Analysis panel -------------------------------------------------------------

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
    progressr::withProgressShiny(
      message = "Calculation in progress",
      detail = "This may take a while ...",
      value = 0,
      expr = {
        react_values$frames_output <- emphazis::proccess_video(
          video_path = video_path,
          frames_path = temp_frames_path,
          subject_model = react_values$subject_model,
          coord1 = coord_1,
          coord2 = coord_2
        )
      }
    )


    message("Mid analysis")

    react_values$dist_table <- emphazis::calculate_distances(react_values$frames_output)

    message("Finished analysis")

  })

  output$analysis_summary <- shiny::renderTable({

    shiny::req(react_values$dist_table)

    head(react_values$dist_table)

  })

  # Plots pane -----------------------------------------------------------------
  output$plot_track <- shiny::renderPlot({

    shiny::req(
      react_values$dist_table
    )

    emphazis::plot_track(
      dist_table = react_values$dist_table,
      color = input$color_subject_1,
      range = input$frame_range
    )
  })

  output$plot_track_heatmap <- shiny::renderPlot({

    shiny::req(
      react_values$dist_table
    )

    emphazis::plot_track_heatmap(
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
