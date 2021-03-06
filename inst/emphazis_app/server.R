# Define server logic ----
server <- function(
  input,
  output,
  session
) {

  `%>%` <- dplyr::`%>%`
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
  react_values$first_frame_path <- NULL

  react_values$arena_x1 <- 0
  react_values$arena_y1 <- 0
  react_values$arena_x2 <- 0
  react_values$arena_y2 <- 0

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

  output$video_description <- shiny::renderTable({
      shiny::req(input$input_video)

      file <- input$input_video
      video_path <- file$datapath

      video_details <- dplyr::bind_rows(
        tibble::tibble(
          var = c("File type", "Size"),
          value = c(
            base::as.character(file$type),
            glue::glue("{round(file$size / (1024^2), 2)} MB")
          )
        ),
        emphazis::extract_video_info(video_path = video_path)
      )
      video_details
    },
    bordered = TRUE,
    colnames = FALSE,
    digits = 2
  )

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

  shiny::observeEvent(input$run_video_process, {
      shiny::req(input$input_video)
      video_path <- input$input_video$datapath
      # Upload video
      video_info <- av::av_video_info(video_path)$frames
      frames_output <- emphazis::convert_video_to_image(
        video_path = video_path,
        frames_path = fs::path_temp("frame_1"),
        fps = 0.2
      )

      react_values$first_frame_path <- frames_output[1]

      output$input_first_frame <- shiny::renderImage({
        list(
          src = frames_output[1],
          contentType = "image/jpg",
          width = video_info$video$width*2,
          height = video_info$video$height*2,
          alt = "First frame image"
        )
      }, deleteFile = FALSE)
    },
    once = TRUE
  )

  # Selection panel -----------------------------------------------------------

  output$input_cut_frame <- shiny::renderImage({

    shiny::req(react_values$first_frame_path)

    list(
      src = react_values$first_frame_path,
      contentType = "image/jpg",
      alt = "Arena selection image"
    )
    }, deleteFile = FALSE
  )

  shiny::observeEvent(input$arena_brush, {
    shiny::req(input$arena_coord_radio == 3)
    react_values$arena_x1 <- round(as.numeric(input$arena_brush$xmin), 0)
    react_values$arena_x2 <- round(as.numeric(input$arena_brush$xmax), 0)
    react_values$arena_y1 <- round(as.numeric(input$arena_brush$ymin), 0)
    react_values$arena_y2 <- round(as.numeric(input$arena_brush$ymax), 0)
  })

  shiny::observeEvent(input$arena_click, {
    shiny::req(input$arena_coord_radio == 1)
    react_values$arena_x1 <- round(as.numeric(input$arena_click$x), 0)
    react_values$arena_y1 <- round(as.numeric(input$arena_click$y), 0)
  })

  shiny::observeEvent(input$arena_click, {
    shiny::req(input$arena_coord_radio == 2)
    react_values$arena_x2 <- round(as.numeric(input$arena_click$x), 0)
    react_values$arena_y2 <- round(as.numeric(input$arena_click$y), 0)
  })

  output$arena_coord_info <- shiny::renderTable({
    tibble::tibble(
      Coord = c("1", "2"),
      X = c(react_values$arena_x1, react_values$arena_x2),
      Y = c(react_values$arena_y1, react_values$arena_y2)
    )},
    digits = 0
  )

  shiny::observeEvent(input$cut_arena_button, {

    shiny::req(react_values$first_frame_path)
    emphazis::slice_image(

    )
  })

  shiny::observeEvent(input$restart_arena_button, {
    output$input_cut_frame <- shiny::renderImage({
      shiny::req(react_values$first_frame_path)
      list(
        src = react_values$first_frame_path,
        contentType = "image/jpg",
        alt = "Arena selection image"
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
