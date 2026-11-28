# SERVER -----------------------------------------------------------------------

# Define server logic required
server <- function(input, output, session) {
  # Filter dataset
  data <- reactive({
    df <- dig.df %>%
      filter(
        SEX %in% input$sexInput,
        TRTMT %in% input$TrtmtArmInput,
        RACE %in% input$EthnicityInput,
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
        DIURETK %in% input$diuretkInput,
        DIURET %in% input$diuretInput,
        KSUPP %in% input$ksuppInput,
        ACEINHIB %in% input$aceinhibInput,
        NITRATES %in% input$nitratesInput,
        HYDRAL %in% input$hydralInput,
        VASOD %in% input$vasodInput,
        DIGDOSE >= input$digdoseInput[1],
        DIGDOSE <= input$digdoseInput[2],
        DEATHREASON %in% input$deathreasonInput
      )
    # Generate message if the sidebar filters leave no patients, otherwise return the filtered data
    validate(need(nrow(df) > 0, "No patients match the current filters."))
    df
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
  
  # Function to use on CheckboxGroupInput to reselect a selection when all unticked
  require_one <- function(inputid, re_selections) {
    observeEvent(input[[inputid]], {
      if (length(input[[inputid]]) == 0) {
        updateCheckboxGroupInput(session, inputid, selected = re_selections)
        } 
      }, ignoreNULL = FALSE)
    }
  require_one("sexInput", c("Male", "Female"))
  require_one("TrtmtArmInput", c("Placebo", "Treatment"))
  require_one("EthnicityInput", c("White", "Other"))
  require_one("diabetesInput", c("No", "Yes"))
  require_one("htnInput", c("No", "Yes"))
  require_one("anginaInput", c("No", "Yes"))
  require_one("prevmiInput", c("No", "Yes"))
  require_one("diguseInput", c("No", "Yes"))
  require_one("diuretkInput", c("No", "Yes"))
  require_one("diuretInput", c("No", "Yes"))
  require_one("ksuppInput", c("No", "Yes"))
  require_one("aceinhibInput", c("No", "Yes"))
  require_one("nitratesInput", c("No", "Yes"))
  require_one("hydralInput", c("No", "Yes"))
  require_one("vasodInput", c("No", "Yes"))
  
  demographics_server(input, output, session, data)
  outcomes_server(input, output, session, data)
  }
