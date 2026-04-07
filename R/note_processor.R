library(dplyr)

# PRIMARY INTERFACE FUNCTION
# Takes parsed note object from `parse_musicxml()` and
# calls all functions below properly on a parsed notes df
# and returns a df with everything needed for app
build_measure_summary <- function(notes) {
    pitch_data <- collect_pitch_classes(notes)
    chords <- sapply(pitch_data$pitch_classes, detect_measure_chord)

    chord_data <- data.frame(
        measure = pitch_data$measure,
        chord = chords,
        family = chord_family(chords),
        stringsAsFactors = FALSE
    )

    note_counts <- measure_note_counts(notes)

    left_join(pitch_data, chord_data, by = "measure") |>
        left_join(note_counts, by = "measure")
}

# adds in an extra family column for use in plotting
chord_family <- function(chord) {
    ifelse(chord == "Unknown", "Unknown", sub("^.* ", "", chord))
}

# Collect pitch classes by measure
# measure with the unique pitch classes found in that measure
collect_pitch_classes <- function(notes) {
    # remove rests and NAs
    note_rows <- notes[!notes$is_rest & !is.na(notes$pitch), ]

    if (!nrow(note_rows)) {
        return(data.frame(
            measure = integer(),
            pitch_classes = I(list()),
            stringsAsFactors = FALSE
        ))
    }

    # discard numbers and plot pitches by measure
    note_rows$pitch_class <- gsub("[0-9]+$", "", note_rows$pitch)
    measure_split <- split(note_rows$pitch_class, note_rows$measure)

    rows <- lapply(names(measure_split), function(measure) {
        # collect the notes and short and reinsert
        pitch_classes <- sort(unique(measure_split[[measure]]))

        data.frame(
            measure = as.integer(measure),
            pitch_classes = I(list(pitch_classes)),
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, rows)
}

# makes df with measures and number of notes (excluding rests)
measure_note_counts <- function(notes) {
    note_rows <- notes[!notes$is_rest & !is.na(notes$pitch), ]

    if (!nrow(note_rows)) {
        return(data.frame(
            measure = integer(),
            note_events = integer(),
            stringsAsFactors = FALSE
        ))
    }

    counts <- as.data.frame(table(note_rows$measure), stringsAsFactors = FALSE)
    names(counts) <- c("measure", "note_events")
    counts$measure <- as.integer(as.character(counts$measure))
    counts
}

# Detect a chord label for !!one measure!!
# Takes one vector of pitch classes and returns a simple triad label.
# This first version only checks major, minor, diminished, and augmented triads.
# If no clear match is found, it returns "Unknown".
detect_measure_chord <- function(pitch_classes) {
    pitch_classes <- unique(na.omit(pitch_classes))

    if (length(pitch_classes) < 3) {
        return("Unknown")
    }

    pitch_map <- c(
        "C" = 0, "B#" = 0,
        "C#" = 1, "Db" = 1,
        "D" = 2,
        "D#" = 3, "Eb" = 3,
        "E" = 4, "Fb" = 4,
        "E#" = 5, "F" = 5,
        "F#" = 6, "Gb" = 6,
        "G" = 7,
        "G#" = 8, "Ab" = 8,
        "A" = 9,
        "A#" = 10, "Bb" = 10,
        "B" = 11, "Cb" = 11
    )

    pitch_numbers <- unname(pitch_map[pitch_classes])
    pitch_numbers <- sort(unique(na.omit(pitch_numbers)))

    roots <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    triads <- list(
        major = c(0, 4, 7),
        minor = c(0, 3, 7),
        diminished = c(0, 3, 6),
        augmented = c(0, 4, 8)
    )

    # search exact match in root 1st
    for (root_index in seq_along(roots)) {
        root_number <- root_index - 1

        # test for match to every triad
        for (quality in names(triads)) {
            candidate <- sort((root_number + triads[[quality]]) %% 12)

            if (setequal(pitch_numbers, candidate)) {
                return(paste(roots[[root_index]], quality))
            }
        }
    }

    # search if triad is subset of pitch classes
    for (root_index in seq_along(roots)) {
        root_number <- root_index - 1

        for (quality in names(triads)) {
            candidate <- sort((root_number + triads[[quality]]) %% 12)

            if (all(candidate %in% pitch_numbers)) {
                return(paste(roots[[root_index]], quality))
            }
        }
    }

    "Unknown"
}
