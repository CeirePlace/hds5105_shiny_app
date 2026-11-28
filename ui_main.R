# UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "green",
  
  ## HEADER ---------------------------------------------------------------------
  
  dashboardHeader(
    title = "DIG Trial",
    titleWidth = 230,
    # Add buttons for ABOUT, PATIENTS & OUTCOMES in the header
    tags$li(class = "dropdown", actionLink("about", "ABOUT", icon = icon("circle-info"), class = "header-nav",
                                           style = "color: white; background-color: #008d4c; border: 2px solid #00a65a; border-radius: 4px;  margin: 8px 4px; padding: 5px 12px;")),
    tags$li(class = "dropdown", actionLink("patients", "PATIENTS", icon = icon("user"), class = "header-nav",
                                           style = "color: white; background-color:#008d4c; border: 2px solid #00a65a; border-radius: 4px;  margin: 8px 4px; padding: 5px 12px;")),
    tags$li(class = "dropdown", actionLink("Outcomes", "OUTCOMES", icon = icon("ambulance"), class = "header-nav",
                                           style = "color: white; background-color: #008d4c;border: 2px solid #00a65a; border-radius: 4px;  margin: 8px 4px; padding: 5px 12px;")),
    
    dropdownMenu(
      type = "notifications", 
      headerText = strong("APP NAVIGATION"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = "Use top buttons to switch pages",
        icon = icon("bars")
      ),
      notificationItem(
        text = "Use sidebar menu to filter patients",
        icon = icon("filter")
      ),
      notificationItem(
        text = "Pick a demographic in PATIENTS page",
        icon = icon("users")
      ),
      notificationItem(
        text = "Pick an event in OUTCOMES page",
        icon = icon("user-doctor")
      ),
      notificationItem(
        text = "Hover over interactive plots for details",
        icon = icon("hand-pointer")
      )
    ),
    tags$li(
      a(
        icon("github", class = "fa-lg"),
        href = "https://github.com/CeirePlace/hds5105_shiny_app.git",
        title = "",
        style = "color: #24292f;"
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
    # Keep title bar and sidebar fixed in place when scrolling, 
    # and change colour of headear buttons when hovering over them
    tags$style(HTML("
      .main-header { position: fixed; width: 100%; z-index: 1030; }
      .main-sidebar { position: fixed; height: 100vh; overflow-y: auto; }
      .content-wrapper { padding-top: 50px; }
      .header-nav:hover { background-color: #007a42 !important; }
    ")),
    
    # Three boxes with Number of patients, number (%) who died & hospitalised (all-causes)
    fluidRow(
      valueBoxOutput("n_box", width = 4),
      valueBoxOutput("death_box", width = 4),
      valueBoxOutput("hosp_box", width = 4)
    ),
    br(),
    # Two hidden tabs (making the three buttons in the header switch between pages)
    tabsetPanel(id= "view", type = "hidden",
                about_view,
                patients_view,
                outcome_view
    )
  )
)

    
    
                