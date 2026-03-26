path <- "Arabesque_L._66_No._1_in_E_Major/score.xml"

doc <- xml2::read_xml(path)
part <- xml2::xml_find_first(doc, ".//part")
measures <- xml2::xml_find_all(part, "./measure")

# print(part)
print(measures)
measure_node <- measures[[1]]
for (child in xml2::xml_children(measure_node)) {
    print("================================")
    print("================================")
    print("================================")
    print(child)
}
