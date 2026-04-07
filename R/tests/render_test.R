library(shiny)

render_test <- function(path = "sheets/Arabesque, Op. 66, No. 1/score.xml") {
  source("R/sheet_drawer.R")

  shinyApp(
    ui = fluidPage(
      osmd_ui()
    ),
    server = function(input, output, session) {
      session$onFlushed(function() {
        render_osmd(session, path)
      }, once = TRUE)
    }
  )
}

print(render_test())
