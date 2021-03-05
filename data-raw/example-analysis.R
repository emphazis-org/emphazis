# library(emphazis)

subject_path <- fs::path_package("emphazis", "extdata", "subject.jpg")
background_path <- fs::path_package("emphazis", "extdata", "background.jpg")
model_test <- generate_subject_model(subject_path, background_path)

#video_path <- fs::path_package("emphazis", "extdata", "sample_rec_10s.mp4")
video_path <- "data-raw/ives-data/movie_sample.mp4"

temp_frames_path <- fs::path_temp("frames")

frames_output <- proccess_video(
  video_path = video_path,
  frames_path = temp_frames_path,
  subject_model = model_test,
  coord1 = c(285, 655),
  coord2 = c(475, 20)
)

# Summarize data
dist_table <- calculate_distances(frames_output)

frame_range <- c(0, length(unique(dplyr::pull(dist_table, "frame"))))

# Plots
p1 <- plot_track(
  dist_table = dist_table,
  color = "green",
  range = frame_range
)

p2 <- plot_track_heatmap(
  dist_table = dist_table,
  range = frame_range
)

p3 <- plot_cumulative_distance(
  dist_table = dist_table,
  range = frame_range
)
p4 <- plot_average_speed(
  dist_table = dist_table,
  range = frame_range
)


library(patchwork)
(p1 + p2) / (p3 + p4)



