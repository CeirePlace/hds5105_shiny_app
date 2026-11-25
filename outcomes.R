## OUTCOMES --------------------------------------------------------------- 
outcome_view <- tabPanelBody("outcome_view",
             selectInput("patient_outcomes", "Select Patient Event:",
                         c("Death" = "DEATH",
                           "Cardiovascular Disease Hospitalisation" = "CVD",
                           "Worsening Heart Failure Hospitalisation" = "WHF")),
             checkboxGroupInput("select_group", "Treatment group:",
                                choices = c("Placebo", "Treatment"),
                                selected = c("Placebo", "Treatment"),
                                inline = TRUE),
             fluidRow(
               column(width = 6, uiOutput("plot_event_ui")),
               column(width = 6, uiOutput("plot_KMcurves_ui"))
             ),
)
