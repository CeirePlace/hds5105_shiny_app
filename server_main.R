# SERVER -----------------------------------------------------------------------

# Define server logic required
server <- function(input, output, session) {
  # Filter dataset
  data <- reactive({
    dig.df %>%
      filter(
        SEX %in% input$sexInput,
        TRTMT %in% input$TrtmtArmInput,
        RACE %in% input$Ethnicity,
        AGE >=input$ageInput[1],
        AGE <=input$ageInput[2]
      )
  })
  
  # Switch to ABOUT view page when about button is clicked
  observeEvent(input$about, {
    updateTabsetPanel(session, "view", selected = "about_view")
    
  })
  # Switch to PATIENTS page when patients button is clicked
  observeEvent(input$patients, {
    updateTabsetPanel(session, "view", selected = "patients_view")
    
  })
  # Switch to OUTCOMES page when patients button is clicked  
  observeEvent(input$Outcomes, {
    updateTabsetPanel(session, "view", selected = "outcome_view")
  })
  demographics_server(input, output, session, data)
  outcomes_server(input, output, session, data)
  }
