osmd_ui <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(
        src = paste0(
          "https://cdn.jsdelivr.net/npm/opensheetmusicdisplay@1.9.3",
          "/build/opensheetmusicdisplay.min.js"
        )
      ),
      shiny::tags$script(shiny::HTML("
        let osmd;
        Shiny.addCustomMessageHandler('osmd-render', async ({xml}) => {
          const el = document.getElementById('osmd');
          if (!el) return;
          if (!osmd) osmd = new opensheetmusicdisplay
            .OpenSheetMusicDisplay(el, { backend: 'svg' });
          el.innerHTML = '';
          await osmd.load(xml);
          osmd.render();
        });
      "))
    ),
    shiny::tags$div(id = "osmd", style = "min-height:900px;overflow-x:auto;")
  )
}

render_osmd <- function(session, path) {
  session$sendCustomMessage(
    "osmd-render",
    list(
      xml = paste(
        readLines(path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    )
  )
}
