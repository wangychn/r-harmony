library(shiny)

# using a typescript library called OSMD to render sheet
# this is why I need a embedded html code chunk
# More info here: https://github.com/opensheetmusicdisplay/opensheetmusicdisplay
osmd_ui <- function() {
  tagList(
    tags$head(
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/opensheetmusicdisplay@1.9.3/build/opensheetmusicdisplay.min.js"
      ),
      tags$script(HTML("
        let osmd;
        Shiny.addCustomMessageHandler(
          'osmd-render',
          async ({xml, start_measure, end_measure}) => {
            const el = document.getElementById('osmd');
            if (!el) return;
            if (!osmd) osmd = new opensheetmusicdisplay
              .OpenSheetMusicDisplay(el, { backend: 'svg' });
            osmd.setOptions({
              drawFromMeasureNumber: start_measure,
              drawUpToMeasureNumber: end_measure
            });
            el.innerHTML = '';
            await osmd.load(xml);
            osmd.render();
          }
        );
      "))
    ),
    tags$div(id = "osmd", style = "min-height:900px;overflow-x:auto;")
  )
}

render_osmd <- function(session, path, start_measure = 1, end_measure = 8) {
  session$sendCustomMessage(
    "osmd-render",
    list(
      start_measure = start_measure,
      end_measure = end_measure,
      xml = paste(
        readLines(path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    )
  )
}
