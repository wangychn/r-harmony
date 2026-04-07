library(patchwork)

source("R/parse_musicxml.R")
source("R/note_processor.R")
source("R/generate_plots.R")

path <- "sheets/Arabesque, Op. 66, No. 1/score.xml"
notes <- parse_musicxml(path)

# cat("rows:", nrow(notes), "\n")
# cat("measures:", min(notes$measure), "to", max(notes$measure), "\n")
# cat("staffs:", paste(sort(unique(notes$staff)), collapse = ", "), "\n")
# cat("rests:", sum(notes$is_rest), "\n")
# cat("notes:", sum(!notes$is_rest), "\n\n")

# print(head(notes, 10))

pitch_data <- collect_pitch_classes(notes)
# print(head(pitch_data, 10))

# print(plot_pitch_class(pitch_data))

pitch_data$chord <- sapply(
    pitch_data$pitch_classes,
    detect_measure_chord
)

print(head(pitch_data, 10))

p1 <- plot_chord_freq(pitch_data)
p2 <- plot_pitch_class(pitch_data)
p3 <- plot_chord_timeline(pitch_data)
p4 <- plot_pitch_density(notes)

combined_plot <- wrap_plots(
    p1, p2, p3, p4,
    ncol = 2
)

print(combined_plot)
