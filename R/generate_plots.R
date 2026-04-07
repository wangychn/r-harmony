library(ggplot2)

# ========= custom theme ==========
pitch_levels <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

pitch_palette <- c(
    "C" = "#264653",
    "Db" = "#2A9D8F",
    "D" = "#3A86A8",
    "Eb" = "#577590",
    "E" = "#7B6D8D",
    "F" = "#E9C46A",
    "Gb" = "#F4A261",
    "G" = "#E76F51",
    "Ab" = "#D62828",
    "A" = "#9C6644",
    "Bb" = "#6D597A",
    "B" = "#355070"
)

chord_palette <- c(
    "major" = "#cbcb05",
    "minor" = "#147ad2",
    "diminished" = "#009f9d",
    "augmented" = "#ff9900",
    "Unknown" = "#9A8C98"
)

music_theme <- function() {
    theme_bw() +
        theme(
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "#E6E1D8"),
            panel.background = element_rect(fill = "#FBF8F2"),
            # plot.background = element_rect(fill = "#FBF8F2", color = NA)
        )
}

# ========= plotting logic ==========
# input: row -> per chord; col -> measure, pitch_classes, chord
# x: chords
# y: number detected
# shows how harmonies dominate selected piece; bar chat
plot_chord_freq <- function(chord_data) {
    counts <- as.data.frame(table(chord_data$chord), stringsAsFactors = FALSE)
    names(counts) <- c("chord", "count")
    counts$family <- chord_family(counts$chord)

    ggplot(
        counts,
        aes(x = reorder(chord, -count), y = count, fill = family)
    ) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = chord_palette) +
        labs(
            title = "Chord Frequency",
            x = "Chord",
            y = "Count"
        ) +
        music_theme() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
}

# input: output from collect_pitch_classes
# x pitch classes
# y frequency
# shows which pitch class dominate; bar chart
# pitch class basically turns all notes into 12 categories
plot_pitch_class <- function(notes) {
    counts <- as.data.frame(
        table(unlist(notes$pitch_classes, use.names = FALSE)),
        stringsAsFactors = FALSE
    )
    names(counts) <- c("pitch_class", "count")
    counts$pitch_class <- factor(counts$pitch_class, levels = pitch_levels)

    ggplot(
        counts,
        aes(x = pitch_class, y = count, fill = pitch_class)
    ) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = pitch_palette) +
        labs(
            title = "Pitch Class Distribution",
            x = "Pitch Class",
            y = "Count"
        ) +
        music_theme()
}

# input: row -> per chord; col -> measure, pitch_classes, chord
# x measures
# y categorical timeline of chord by measure
# color for chord family; overlapping line chart
plot_chord_timeline <- function(chord_data) {
    ggplot(
        chord_data,
        aes(x = measure, y = 1, fill = family)
    ) +
        geom_tile() +
        scale_fill_manual(values = chord_palette) +
        labs(
            title = "Chord Timeline",
            x = "Measure",
            y = NULL,
            fill = "Family"
        ) +
        music_theme() +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank()
        )
}

# input: output from collect_pitch_classes
# x measures
# y number of note events
plot_pitch_density <- function(notes) {
    ggplot(notes, aes(x = measure, y = note_events)) +
        geom_line(color = "#264653", linewidth = 0.8) +
        geom_point(color = "#E76F51", size = 2) +
        labs(
            title = "Pitch Density by Measure",
            x = "Measure",
            y = "Note Events"
        ) +
        music_theme()
}
