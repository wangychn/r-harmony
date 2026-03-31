library(shiny)

render_test <- function(path = "sheets/Arabesque_L._66_No._1_in_E_Major/score.xml") {
  source("R/sheet_drawer.R")

  shiny::shinyApp(
    ui = shiny::fluidPage(
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
