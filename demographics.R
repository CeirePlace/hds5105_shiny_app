# UI ---------------------------------------------------------------        
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

# SERVER -----------------------------------------------------
demographics_server <- function(input, output, session, data) {
  output$age_col <- renderPlotly({
    age_counts <- data() %>%
      summarise(.by = c(TRTMT, AGE), n = n())
    plt1 <- ggplot(age_counts, 
                   aes(x = AGE, 
                       y = n, 
                       fill = TRTMT,
                       text = sprintf("Treatment: %s<br>Age: %d<br>Count: %d", TRTMT, AGE, n))) +
      geom_col(position = "identity", alpha = 0.5) +
      scale_fill_manual(values = c("Placebo" = colour1, 
                                   "Treatment" = colour2)) +
      labs(x = "Age (years)", 
           y = "Number of Participants", 
           fill = "Treatment Arm") +
      theme_bw()
    ggplotly(plt1, tooltip = "text")
    })
}