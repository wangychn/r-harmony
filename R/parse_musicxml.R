# Parse a MusicXML file into a note-level data frame
# Every column -> one note/rest
# Columns in the returned data frame:
# - measure: measure number
# - staff: staff number
# - onset: start position within the measure
# - duration: note or rest duration
# - pitch: pitch label such as C4 or Bb5
# - is_rest: TRUE if the event is a rest, FALSE otherwise
parse_musicxml <- function(path) {
  empty <- data.frame(
    measure = integer(),
    staff = integer(),
    onset = numeric(),
    duration = numeric(),
    pitch = character(),
    is_rest = logical(),
    stringsAsFactors = FALSE
  )

  doc <- xml2::read_xml(path)
  measures <- xml2::xml_find_all(doc, ".//part[1]/measure")

  rows <- list()
  divisions <- 1

  # sweep through each measure
  for (measure_index in seq_along(measures)) {
    measure_node <- measures[[measure_index]]
    measure_number <- as.integer(xml2::xml_attr(measure_node, "number"))
    if (is.na(measure_number)) measure_number <- measure_index
    position <- 0
    last_onset <- 0

    # go through all relevant sub elements of measure
    # attributes: measure divisions
    # backup: moves current time (necessary for left hand)
    for (child in xml2::xml_children(measure_node)) {
      name <- xml2::xml_name(child)

      if (name == "attributes") {
        divisions_node <- xml2::xml_find_first(child, "./divisions")
        has_divisions <- !inherits(divisions_node, "xml_missing")

        if (has_divisions) {
          divisions <- as.numeric(xml2::xml_text(divisions_node))
        }

        next
      }

      if (name == "backup") {
        backup_duration_node <- xml2::xml_find_first(child, "./duration")
        backup_duration <- as.numeric(xml2::xml_text(backup_duration_node))
        position <- position - backup_duration
        next
      }

      if (name == "forward") {
        forward_duration_node <- xml2::xml_find_first(child, "./duration")
        forward_duration <- as.numeric(xml2::xml_text(forward_duration_node))
        position <- position + forward_duration
        next
      }

      if (name == "note") {
        duration_node <- xml2::xml_find_first(child, "./duration")
        duration <- as.numeric(xml2::xml_text(duration_node))
        chord_node <- xml2::xml_find_first(child, "./chord")
        rest_node <- xml2::xml_find_first(child, "./rest")
        has_chord <- !inherits(chord_node, "xml_missing")
        is_rest <- !inherits(rest_node, "xml_missing")

        onset <- if (has_chord) last_onset else position

        if (!has_chord) {
          position <- position + duration
        }

        last_onset <- onset

        alter_node <- xml2::xml_find_first(child, "./pitch/alter")

        alter <- "0"
        alter_exists <- !inherits(alter_node, "xml_missing")
        if (alter_exists) {
          alter <- xml2::xml_text(alter_node)
        }

        accidental <- switch(alter,
          "-2" = "bb",
          "-1" = "b",
          "0" = "",
          "1" = "#",
          "2" = "x",
          ""
        )
        pitch <- if (is_rest) {
          NA_character_
        } else {
          step_node <- xml2::xml_find_first(child, "./pitch/step")
          octave_node <- xml2::xml_find_first(child, "./pitch/octave")
          step <- xml2::xml_text(step_node)
          octave <- xml2::xml_text(octave_node)

          paste0(
            step,
            accidental,
            octave
          )
        }

        staff_node <- xml2::xml_find_first(child, "./staff")
        staff <- as.integer(xml2::xml_text(staff_node))

        rows[[length(rows) + 1]] <- data.frame(
          measure = measure_number,
          staff = staff,
          onset = onset / divisions,
          duration = duration / divisions,
          pitch = pitch,
          is_rest = is_rest,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(rows)) {
    return(empty)
  }
  do.call(rbind, rows)
}
