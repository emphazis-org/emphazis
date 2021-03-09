# library(emphazis)
library(dplyr)
subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
model_test <- generate_subject_model(subject_path, background_path)

#video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
video_path <- "data-raw/ives-data/movie_sample.mp4"

temp_frames_path <- fs::path_temp("frames")


first_frame_path <- convert_video_to_image(
  video_path = video_path,
  frames_path = fs::path_temp("frames2"),
  fps = 5
)

first_frame <- first_frame_path[1]
# image_path <- first_frame
n_frames <- av::av_video_info(video_path)$video$frames

position_table <- proccess_video(
  video_path = video_path,
  frames_path = temp_frames_path,
  subject_model = model_test,
  coord1 = c(285, 20),
  coord2 = c(475, 655),
  fps = 3
) %>%
  progressr::with_progress()

attributes(position_table)

# Summarize data
metrics_table <- calculate_metrics(
  position_table = position_table
)
attributes(metrics_table)$unit

conversion_rate_width <- convert_image_size_unit(
  image_path = first_frame,
  width = 21,
  height = NULL
)

conversion_rate_height <- convert_image_size_unit(
  image_path = first_frame,
  width = NULL,
  height = 18
)

converted_table <- convert_table_unit(
  metrics_table,
  conversion_rate_width = conversion_rate_width,
  conversion_rate_height = conversion_rate_height,
  unit = "cm"
)
attributes(converted_table)$unit

analysis_summary(
  metrics_table = converted_table
)

frame_range <- c(0, length(unique(dplyr::pull(metrics_table, "frame"))))

# Plots
p1 <- plot_track(
  metrics_table = metrics_table,
  color = "green",
  range = frame_range
)

p2 <- plot_track_heatmap(
  metrics_table = metrics_table,
  range = frame_range
)

p3 <- plot_cumulative_distance(
  metrics_table = metrics_table,
  range = frame_range
)
p4 <- plot_average_speed(
  metrics_table = metrics_table,
  range = frame_range
)

library(patchwork)
(p1 + p2 + plot_layout(widths = 4)) / (p3 + p4 + plot_layout(widths = 1))



