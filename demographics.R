## PATIENTS ---------------------------------------------------------------        
demographics_menu <- menuItem("Demographics",
                              icon = icon("circle-info"),
                              checkboxGroupInput(
                                "sexInput",
                                label = "Sex",
                                choices = unique(dig.df$SEX),
                                selected = unique(dig.df$SEX),
                                inline = TRUE),
                              sliderInput(
                                "ageInput", 
                                label = "Age",
                                value = c(min(dig.df$AGE, na.rm = TRUE), max(dig.df$AGE, na.rm=TRUE)),
                                min = min(dig.df$AGE, na.rm = TRUE),
                                max = max(dig.df$AGE, na.rm=TRUE),
                                step = 1,
                                sep = ""
                                ),
                              checkboxGroupInput(
                                "TrtmtArmInput",
                                label = "Treatment Arm",
                                choices = levels(dig.df$TRTMT),
                                selected = levels(dig.df$TRTMT),
                                inline = TRUE
                                ),
                              checkboxGroupInput(
                                inputId = "Ethnicity",
                                label = "Ethnicity",
                                choices = levels(dig.df$RACE),
                                selected = levels(dig.df$RACE) 
                                )
                              )

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
