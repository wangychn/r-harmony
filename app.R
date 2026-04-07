library(shiny)
source("R/sheet_drawer.R")
source("R/parse_musicxml.R")
source("R/note_processor.R")
source("R/generate_plots.R")

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
    conditionalPanel(
        condition = "input.view_mode == 'Main'",
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
                    h3("Selected Chords"),
                    tableOutput("range_chords")
                )
            )
        )
    ),
    conditionalPanel(
        condition = "input.view_mode == 'Breakdown'",
        fluidRow(
            column(
                width = 12,
                wellPanel(
                    plotOutput("breakdown_plot", height = 900)
                )
            )
        )
    )
)

server <- function(input, output, session) {
    active_music_path <- reactive({
        if (!is.null(input$upload_piece)) {
            input$upload_piece$datapath
        } else {
            req(nzchar(input$piece))
            input$piece
        }
    })

    # ======= loading in the music data ========
    notes <- reactive({
        parse_musicxml(active_music_path())
    })

    pitch_data <- reactive({
        collect_pitch_classes(notes())
    })

    chord_data <- reactive({
        data <- pitch_data()
        data$chord <- sapply(data$pitch_classes, detect_measure_chord)
        data
    })

    selected_notes <- reactive({
        req(input$measure_range)
        notes()[
            notes()$measure >= input$measure_range[1] &
                notes()$measure <= input$measure_range[2],
        ]
    })

    selected_pitch_data <- reactive({
        req(input$measure_range)
        pitch_data()[
            pitch_data()$measure >= input$measure_range[1] &
                pitch_data()$measure <= input$measure_range[2],
        ]
    })

    selected_chord_data <- reactive({
        req(input$measure_range)
        chord_data()[
            chord_data()$measure >= input$measure_range[1] &
                chord_data()$measure <= input$measure_range[2],
        ]
    })

    # ========================================

    # update the size of the slider based on current selected piece
    observe({
        measures <- notes()$measure
        req(length(measures))

        min_measure <- min(measures)
        max_measure <- max(measures)

        updateSliderInput(
            session = session,
            inputId = "measure_range",
            min = min_measure,
            max = max_measure,
            value = c(min_measure, min(max_measure, min_measure + 3))
        )
    })

    # this is added to render main page when switching back to it from breakdown
    observeEvent(
        list(input$view_mode, input$measure_range, active_music_path()),
        {
            req(input$view_mode == "Main")
            req(input$measure_range)

            render_osmd(
                session = session,
                path = active_music_path(),
                start_measure = input$measure_range[1],
                end_measure = input$measure_range[2]
            )
        }
    )

    output$range_chords <- renderTable({
        req(nrow(selected_chord_data()) > 0)
        selected_chord_data()[, c("measure", "chord")]
    })

    output$breakdown_plot <- renderPlot({
        req(nrow(selected_chord_data()) > 0)

        patchwork::wrap_plots(
            plot_chord_freq(selected_chord_data()),
            plot_pitch_class(selected_pitch_data()),
            plot_chord_timeline(selected_chord_data()),
            plot_pitch_density(selected_notes()),
            ncol = 2
        )
    })
}

shinyApp(ui, server)
