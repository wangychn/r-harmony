# input: row -> per chord; col -> measure, pitch_classes, chord
# x: chords
# y: number detected
# shows how harmonies dominate selected piece; bar chat
plot_chord_freq <- function(chord_data) {
    counts <- as.data.frame(table(chord_data$chord), stringsAsFactors = FALSE)
    names(counts) <- c("chord", "count")

    ggplot2::ggplot(
        counts,
        ggplot2::aes(x = stats::reorder(chord, -count), y = count, fill = chord)
    ) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::labs(
            title = "Chord Frequency",
            x = "Chord",
            y = "Count"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
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

    ggplot2::ggplot(
        counts,
        ggplot2::aes(x = pitch_class, y = count, fill = pitch_class)
    ) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::labs(
            title = "Pitch Class Distribution",
            x = "Pitch Class",
            y = "Count"
        ) +
        ggplot2::theme_minimal()
}

# input: row -> per chord; col -> measure, pitch_classes, chord
# x measures
# y categorical timeline of chord by measure
# color for chord family; overlapping line chart
plot_chord_timeline <- function(chord_data) {
    ggplot2::ggplot(
        chord_data,
        ggplot2::aes(x = measure, y = 1, fill = sub("^.* ", "", chord))
    ) +
        ggplot2::geom_tile() +
        ggplot2::labs(
            title = "Chord Timeline",
            x = "Measure",
            y = NULL,
            fill = "Family"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}


# input: output from collect_pitch_classes
# x measures
# y number of note events
plot_pitch_density <- function(notes) {
    note_rows <- notes[!notes$is_rest & !is.na(notes$pitch), ]
    counts <- as.data.frame(table(note_rows$measure), stringsAsFactors = FALSE)
    names(counts) <- c("measure", "count")
    counts$measure <- as.integer(as.character(counts$measure))

    ggplot2::ggplot(counts, ggplot2::aes(x = measure, y = count)) +
        ggplot2::geom_line(color = "steelblue") +
        ggplot2::geom_point(color = "steelblue") +
        ggplot2::labs(
            title = "Pitch Density by Measure",
            x = "Measure",
            y = "Note Events"
        ) +
        ggplot2::theme_minimal()
}
