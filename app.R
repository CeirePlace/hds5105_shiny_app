source("global.R")        # FIRST — the UI reads dig.df when it's built
source("demographics.R")  # fragments + demographics_server()
source("outcomes.R")      # fragments + outcomes_server()
source("ui_main.R")       # composes the fragments into `ui`
source("server_main.R")   # defines `server`
shinyApp(ui = ui, server = server)

    
      





