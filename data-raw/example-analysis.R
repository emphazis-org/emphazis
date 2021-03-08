# library(emphazis)

subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
model_test <- generate_subject_model(subject_path, background_path)

#video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
video_path <- "data-raw/ives-data/movie_sample.mp4"

temp_frames_path <- fs::path_temp("frames")


frames_path <- convert_video_to_image(
  video_path = video_path,
  frames_path = fs::path_temp("frames2"),
  fps = 5
)
frames_path[1]

n_frames <- av::av_video_info(video_path)$video$frames

frames_path <- convert_video_to_image(
  video_path = video_path,
  frames_path = fs::path_temp("frames2"),
  fps = (1/n_frames)*2
)

frames_output <- proccess_video(
  video_path = video_path,
  frames_path = temp_frames_path,
  subject_model = model_test,
  coord1 = c(285, 655),
  coord2 = c(475, 20)
)

# Summarize data
metrics_table <- calculate_metrics(frames_output)

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
(p1 + p2) / (p3 + p4)



