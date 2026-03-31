library(shiny)
source("R/sheet_drawer.R")

available_scores <- function() {
    paths <- list.files(
        path = "sheets",
        pattern = "\\.xml$",
        recursive = TRUE,
        full.names = TRUE
    )

    labels <- basename(dirname(paths))
    stats::setNames(paths, labels)
}

control_bar_ui <- function() {
    score_choices <- c("Select a piece..." = "", available_scores())

    wellPanel(
        fluidRow(
            column(
                width = 3,
                selectInput(
                    inputId = "piece",
                    label = "Piece",
                    choices = score_choices
                )
            ),
            column(
                width = 3,
                fileInput(
                    inputId = "upload_piece",
                    label = "Upload MusicXML",
                    accept = c(".xml", ".musicxml")
                )
            ),
            column(
                width = 4,
                sliderInput(
                    inputId = "measure_range",
                    label = "Measure Range",
                    min = 1,
                    max = 8,
                    value = c(1, 4),
                    step = 1
                )
            ),
            column(
                width = 2,
                radioButtons(
                    inputId = "view_mode",
                    label = "View",
                    choices = c("Main", "Breakdown"),
                    inline = TRUE
                )
            )
        )
    )
}

ui <- fluidPage(
    control_bar_ui(),
    hr(),
    fluidRow(
        column(
            width = 8,
            wellPanel(
                osmd_ui()
            )
        ),
        column(
            width = 4,
            wellPanel(
                h3("Breakdown View"),
                div("Breakdown plots go here")
            )
        )
    )
)

server <- function(input, output, session) {
    observeEvent(
        list(input$piece, input$measure_range),
        {
            req(nzchar(input$piece))
            render_osmd(
                session = session,
                path = input$piece,
                start_measure = input$measure_range[1],
                end_measure = input$measure_range[2]
            )
        }
    )
}

shinyApp(ui, server)
