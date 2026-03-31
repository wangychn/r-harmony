source("R/parse_musicxml.R")

path <- "sheets/Arabesque_L._66_No._1_in_E_Major/score.xml"
notes <- parse_musicxml(path)

cat("rows:", nrow(notes), "\n")
cat("measures:", min(notes$measure), "to", max(notes$measure), "\n")
cat("staffs:", paste(sort(unique(notes$staff)), collapse = ", "), "\n")
cat("rests:", sum(notes$is_rest), "\n")
cat("notes:", sum(!notes$is_rest), "\n\n")

print(utils::head(notes, 10))
