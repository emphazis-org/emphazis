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

  # Upload panel
  react_values$first_frame_path <- NULL

  # arena panel
  react_values$arena_x1 <- 0
  react_values$arena_y1 <- 0
  react_values$arena_x2 <- 0
  react_values$arena_y2 <- 0

  react_values$arena_slice_path <- NULL


  # subject panel
  react_values$subject_x1 <- 0
  react_values$subject_y1 <- 0
  react_values$subject_x2 <- 0
  react_values$subject_y2 <- 0

  react_values$subject_slice_path <- NULL


  # Statistics panel
  react_values$subject_model <- NULL
  react_values$position_table <- NULL
  react_values$metrics_table <- NULL
  react_values$converted_table <- NULL

  react_values$conversion_rate_width <- NULL
  react_values$conversion_rate_height <- NULL

  react_values$arena_width_unit <- NULL
  react_values$arena_height_unit <- NULL

  # Plots Panel
  react_values$max_frame_value <- NULL

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
      frames_output_path <- emphazis::convert_video_to_image(
        video_path = video_path,
        frames_path = fs::path_temp("frame_1"),
        fps = 0.2
      )

      react_values$first_frame_path <- frames_output_path[1]

      output$input_first_frame <- shiny::renderImage({
        list(
          src = frames_output_path[1],
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

    react_values$arena_slice_path <- emphazis::slice_image(
      image_path = react_values$first_frame_path,
      output_path = fs::path_temp("slice_1"),
      coord1 = c(react_values$arena_x1, react_values$arena_y1),
      coord2 = c(react_values$arena_x2, react_values$arena_y2)
    )
  })

  output$sliced_arena <- shiny::renderImage({
    shiny::req(react_values$arena_slice_path)
    list(
      src = react_values$arena_slice_path,
      contentType = "image/jpg",
      alt = "Arena slice"
    )
    }, deleteFile = FALSE
  )

  shiny::observeEvent(input$restart_arena_button, {
    react_values$arena_slice_path <- NULL
  })

  # Subject selection panel ---------------------------------------------------
  output$subject_select <- shiny::renderImage({

    shiny::req(react_values$arena_slice_path)

    list(
      src = react_values$arena_slice_path,
      contentType = "image/jpg",
      alt = "Arena selection image"
    )
  }, deleteFile = FALSE
  )

  shiny::observeEvent(input$subject_brush, {
    shiny::req(input$subject_coord_radio == 3)
    react_values$subject_x1 <- round(as.numeric(input$subject_brush$xmin), 0)
    react_values$subject_x2 <- round(as.numeric(input$subject_brush$xmax), 0)
    react_values$subject_y1 <- round(as.numeric(input$subject_brush$ymin), 0)
    react_values$subject_y2 <- round(as.numeric(input$subject_brush$ymax), 0)
  })

  shiny::observeEvent(input$subject_click, {
    shiny::req(input$subject_coord_radio == 1)
    react_values$subject_x1 <- round(as.numeric(input$subject_click$x), 0)
    react_values$subject_y1 <- round(as.numeric(input$subject_click$y), 0)
  })

  shiny::observeEvent(input$subject_click, {
    shiny::req(input$subject_coord_radio == 2)
    react_values$subject_x2 <- round(as.numeric(input$subject_click$x), 0)
    react_values$subject_y2 <- round(as.numeric(input$subject_click$y), 0)
  })

  output$subject_coord_info <- shiny::renderTable({
    tibble::tibble(
      Coord = c("1", "2"),
      X = c(react_values$subject_x1, react_values$subject_x2),
      Y = c(react_values$subject_y1, react_values$subject_y2)
    )},
    digits = 0
  )

  shiny::observeEvent(input$cut_subject_button, {

    shiny::req(react_values$arena_slice_path)

    react_values$subject_slice_path <- emphazis::slice_image(
      image_path = react_values$arena_slice_path,
      output_path = fs::path_temp("slice_subject"),
      coord1 = c(react_values$subject_x1, react_values$subject_y1),
      coord2 = c(react_values$subject_x2, react_values$subject_y2)
    )
  })

  output$sliced_subject <- shiny::renderImage({
    shiny::req(react_values$subject_slice_path)
    list(
      src = react_values$subject_slice_path,
      contentType = "image/jpg",
      alt = "Subject slice"
    )
  }, deleteFile = FALSE
  )

  shiny::observeEvent(input$restart_subject_button, {
    react_values$subject_slice_path <- NULL
  })

  # Analysis panel -------------------------------------------------------------
  shiny::observeEvent(input$start_job_button, {

    shiny::req(
      input$input_video,
      input$fps_slider,
      # input$input_subject,
      # input$input_bg
      react_values$arena_slice_path,
      react_values$subject_slice_path,
    )

    message("Start analysis")

    coord_1 <- c(
      react_values$arena_x1,
      react_values$arena_y1
    )

    coord_2 <- c(
      react_values$arena_x2,
      react_values$arena_y2
    )

    message("model start")

    react_values$subject_model <- emphazis::generate_subject_model(
      subject_path = react_values$subject_slice_path,
      background_path = react_values$arena_slice_path
    )
    message("Model finished")

    video_path <- input$input_video$datapath

    temp_frames_path <- fs::path_temp("frames")
    progressr::withProgressShiny(
      message = "Calculation in progress",
      detail = "This may take a while ...",
      value = 0,
      expr = {
        react_values$position_table <- emphazis::proccess_video(
          video_path = video_path,
          subject_model = react_values$subject_model,
          frames_path = temp_frames_path,
          coord1 = coord_1,
          coord2 = coord_2,
          fps = input$fps_slider
        )
      }
    )

    # TODO remove test infrastructure
    # print(react_values$position_table)
    # str(react_values$position_table)
    message("Video finished")
  })


  shiny::observeEvent(react_values$position_table, {
    shiny::req(
      react_values$position_table,
    )
    react_values$metrics_table <- emphazis::calculate_metrics(
      position_table = react_values$position_table
    )

    # TODO remove test infrastructure
    # print(react_values$metrics_table)
    # str(react_values$metrics_table)
    message("Metrics generated")


    # This is for plot frame slider
    react_values$max_frame_value <- max(react_values$metrics_table$frame)
  })
  # ----------------------------------------------------------------------
  # CONVERSION RATES

  # Get conversion rate from measures
  arena_measures_input <- shiny::reactive({
    list(
      input$arena_width,
      input$arena_height
    )
  })

  shiny::observeEvent(arena_measures_input(), {
    shiny::req(
      react_values$arena_slice_path
    )
    react_values$conversion_rate_width <- emphazis::convert_image_size_unit(
      image_path = react_values$arena_slice_path,
      width = input$arena_width,
      height = NULL
    )
    react_values$conversion_rate_height <- emphazis::convert_image_size_unit(
      image_path = react_values$arena_slice_path,
      width = NULL,
      height = input$arena_height
    )
    shiny::updateNumericInput(
      session = session,
      inputId = "conversion_rate_width",
      value = round(react_values$conversion_rate_width, 4)
    )
    shiny::updateNumericInput(
      session = session,
      inputId = "conversion_rate_height",
      value = round(react_values$conversion_rate_height, 4)
    )
    message("conversion measurements block end")
  }, ignoreInit = TRUE
  )

  arena_rates_input <- shiny::reactive({
    list(
      input$conversion_rate_width,
      input$conversion_rate_height
    )
  })
  shiny::observeEvent(arena_rates_input(), {
    shiny::req(
      react_values$arena_slice_path
    )
    react_values$arena_width_unit <- emphazis::rate_to_unit(
      image_path = react_values$arena_slice_path,
      rate = input$conversion_rate_width,
      type = "width"
    )
    react_values$arena_height_unit <- emphazis::rate_to_unit(
      image_path = react_values$arena_slice_path,
      rate = input$conversion_rate_height,
      type = "height"
    )
    shiny::updateNumericInput(
      session = session,
      inputId = "arena_width",
      value = react_values$arena_width_unit
    )
    shiny::updateNumericInput(
      session = session,
      inputId = "arena_height",
      value = react_values$arena_height_unit
    )
    message("conversion rates block end")
  }, ignoreInit = TRUE
  )

#   shiny::observeEvent(input$conversion_rate_width, {
#     shiny::req(
#       input$conversion_rate_width
#     )
#     react_values$conversion_rate_widtht <- input$conversion_rate_width
#
#     #print(react_values$conversion_rate_width)
#     message("conversion measurements block end")
#   })


  #------------------------------------------------------------------
  # Update summary table
  shiny::observeEvent(input$update_summary_button, {

    shiny::req(
      react_values$metrics_table,
      react_values$conversion_rate_width,
      react_values$conversion_rate_height,
      input$conversion_unit_radio
    )

    if (input$conversion_unit_radio == 1) {
      unit_to_convert <- "cm"
    } else if (input$conversion_unit_radio == 2) {
      unit_to_convert <- "in"
    } else if (input$conversion_unit_radio == 3) {
      unit_to_convert <- "px"
    }

    react_values$converted_table <- emphazis::convert_table_unit(
      metrics_table = react_values$metrics_table,
      conversion_rate_width = react_values$conversion_rate_width,
      conversion_rate_height = react_values$conversion_rate_height,
      unit = unit_to_convert
    )

    # TODO remove test infrastructure
    # print(react_values$converted_table)
    # str(react_values$converted_table)
    message("Units converted")


    output$analysis_summary <- shiny::renderTable({

      shiny::req(react_values$converted_table)

      message("Summary rendered")

      emphazis::analysis_summary(
        metrics_table = react_values$converted_table
      )

    })
  })

  # Plots pane -----------------------------------------------------------------

  # Update Slider based on frame number
  shiny::observeEvent(react_values$max_frame_value, {

    req(react_values$max_frame_value)

    shiny::updateSliderInput(
      inputId = "frame_range",
      max = react_values$max_frame_value,
      value = c(1, react_values$max_frame_value)
    )
  })

  output$plot_track <- shiny::renderPlot({

    shiny::req(
      react_values$converted_table
    )

    emphazis::plot_track(
      metrics_table = react_values$converted_table,
      color = input$color_subject_1,
      range = input$frame_range
    )
  })

  output$plot_track_heatmap <- shiny::renderPlot({

    shiny::req(
      react_values$converted_table
    )

    emphazis::plot_track_heatmap(
      metrics_table = react_values$converted_table,
      range = input$frame_range
    )
  })

  output$plot_dist <- shiny::renderPlot({

    shiny::req(react_values$converted_table)

    emphazis::plot_cumulative_distance(
      metrics_table = react_values$converted_table,
      range = input$frame_range
    )
  })

  output$plot_speed <- shiny::renderPlot({

    shiny::req(react_values$converted_table)

    emphazis::plot_average_speed(
      metrics_table = react_values$converted_table,
      range = input$frame_range
    )
  })

  # 3d Plots pane --------------------------------------------------------------
  output$plot_3d_dots <- plotly::renderPlotly({
    shiny::req(react_values$converted_table)

    emphazis::plot_3d_dots(
      metrics_table = react_values$converted_table,
      size = 3
    )

  })

  output$plot_3d_lines <- plotly::renderPlotly({
    shiny::req(react_values$converted_table)

    emphazis::plot_3d_lines(metrics_table = react_values$converted_table)

  })

  output$plot_3d_surface <- plotly::renderPlotly({
    shiny::req(react_values$converted_table)

    emphazis::plot_3d_surface(metrics_table = react_values$converted_table)

  })

}
