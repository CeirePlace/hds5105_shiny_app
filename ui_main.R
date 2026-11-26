# UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  
  ## HEADER ---------------------------------------------------------------------
  
  dashboardHeader(
    title = "DIG Trial",
    titleWidth = 230,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = "instructions 1",
        icon = icon("spinner")
      ),
      notificationItem(
        text = "instructions 2",
        icon = icon("address-card")
      ),
      notificationItem(
        text = "instructions 3",
        icon = icon("calendar")
      ),
      notificationItem(
        text = "instructions 4",
        icon = icon("user-md")
      ),
      notificationItem(
        text = "instructions 5",
        icon = icon("ambulance")
      ),
      notificationItem(
        text = "instructions 6 - add confirm selection button for this",
        icon = icon("exclamation")
      )
    ),
    tags$li(
      a(
        icon("github", class = "fa-lg"),
        href = "https://github.com/CeirePlace/hds5105_shiny_app.git",
        title = "",
      ),
      class = "dropdown"
    )
  ),
  
  ## SIDEBAR --------------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(demographics_menu,
                medical_hx_menu,
                medication_menu,
                mortality_menu)
      ),
  
  ## BODY -----------------------------------------------------------------------
  dashboardBody(
    # Add buttons for PATIENTS & OUTCOMES at the top of the page
    fluidRow(
      column(
        width = 12,
        bsButton("about",
                 label = "ABOUT",
                 icon = icon("circle-info"),
                 style = "success"),
        bsButton("patients", 
                 label = "PATIENTS", 
                 icon = icon("user"), 
                 style = "success"),
        bsButton("Outcomes", 
                 label = "OUTCOMES", 
                 icon = icon("ambulance"), 
                 style = "success"),
      )
    ),
    br(),
    # Three boxes with Number of patients, number (%) who died & hospitalised (all-causes)
    fluidRow(
      valueBoxOutput("n_box", width = 4),
      valueBoxOutput("death_box", width = 4),
      valueBoxOutput("hosp_box", width = 4)
    ),
    br(),
    # Two hidden tabs (making the two buttons switch between pages)
    tabsetPanel(id= "view", type = "hidden",
                about_view,
                patients_view,
                outcome_view
    )
  )
)

    
    
                