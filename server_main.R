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
        AGE >= input$ageInput[1],
        AGE <= input$ageInput[2],
        BMI >= input$bmiInput[1],
        BMI <= input$bmiInput[2],
        SYSBP >= input$sysBPInput[1],
        SYSBP <= input$sysBPInput[2],
        DIABP >= input$diaBPInput[1],
        DIABP <= input$diaBPInput[2],
        DIABETES %in% input$diabetesInput,
        HYPERTEN %in% input$htnInput,
        ANGINA %in% input$anginaInput,
        PREVMI %in% input$prevmiInput,
        DIGUSE %in% input$diguseInput,
        DIURETK %in% inputdiuretkInput,
        DIURET %in% input$diuretInput,
        KSUPP %in% input$ksuppInput,
        ACEINHIB %in% input$aceinhibInput,
        NITRATES %in% input$nitratesInput,
        HYDRAL %in% input$hydralInput,
        VASOD %in% input$vasodInput,
        DIGDOSE >= input$digdoseInput[1],
        DIGDOSE <= input$digdoseInput[2],
        REASON %in% input$reasonInput
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
