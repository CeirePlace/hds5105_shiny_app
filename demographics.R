## PATIENTS ---------------------------------------------------------------        
patients_view <- tabPanelBody("patients_view",
             # ADD all code for patient plots here instead of "ADD PATIENT PLOTS HERE"
             fluidRow(
               box(title = "Distribution by Treatment Group", 
                   status = "primary", 
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   width = 6,
                   plotlyOutput("age_col", 
                                height = 250)),
               box(title = "Medical history by treatment arm", 
                   status = "primary", 
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   width = 6,
                   plotlyOutput("hx_bar", 
                                height = 250)),
               box(title = "Barchart", 
                   status = "primary", 
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   width = 6,
                   plotlyOutput("barchart", 
                                height = 250))
             )
)
