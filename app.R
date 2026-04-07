library(shiny)
library(shinycssloaders)
library(plotly)
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
    setNames(paths, labels)
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
                    withSpinner(
                        tableOutput("range_chords")
                    )
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
                    withSpinner(
                        plotlyOutput("breakdown_plot", height = 900)
                    )
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

    measure_summary <- reactive({
        build_measure_summary(notes())
    })

    selected_measure_summary <- reactive({
        req(input$measure_range)
        measure_summary()[
            measure_summary()$measure >= input$measure_range[1] &
                measure_summary()$measure <= input$measure_range[2],
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
        req(nrow(selected_measure_summary()) > 0)
        selected_measure_summary()[, c("measure", "chord")]
    })

    output$breakdown_plot <- renderPlotly({
        req(nrow(selected_measure_summary()) > 0)

        subplot(
            ggplotly(plot_chord_freq(selected_measure_summary())),
            ggplotly(plot_pitch_class(selected_measure_summary())),
            ggplotly(plot_chord_timeline(selected_measure_summary())),
            ggplotly(plot_pitch_density(selected_measure_summary())),
            nrows = 2,
            margin = 0.06,
            titleX = TRUE,
            titleY = TRUE
        )
    })
}

shinyApp(ui, server)
