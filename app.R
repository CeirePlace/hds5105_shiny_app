# Install packages if required
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("rintrojs")) install.packages("rintrojs")
if (!require("shinyBS")) install.packages("shinyBS")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("survival")) install.packages("survival")
if (!require("survminer")) install.packages("survminer")

# Load packages
library(shiny)
library(shinydashboard)
library(rintrojs)
library(shinyBS)
library(tidyverse)
library(plotly)
library(survival)
library(survminer)

# Data 
dig.df <-read.csv("data/DIG.csv")

# Clean like in assignment 4 (option to add other variables)

# Select columns and convert to relevant datatype
dig.df <- dig.df %>% 
  select("ID", "TRTMT", "AGE", "SEX", "BMI", 
         "KLEVEL", "CREAT", "DIABP", "SYSBP",
         "HYPERTEN", "CVD", "WHF", "DIG", 
         "HOSP", "HOSPDAYS", "DEATH", "DEATHDAY") %>%
  mutate(ID = as.character(ID),
         TRTMT = as.factor(TRTMT),
         AGE = as.integer(AGE),
         SEX = as.factor(SEX),
         BMI = as.numeric(BMI),
         KLEVEL = as.numeric(KLEVEL),
         CREAT = as.numeric(CREAT),
         DIABP = as.numeric(DIABP),
         SYSBP = as.numeric(SYSBP),
         HYPERTEN = as.factor(HYPERTEN),
         CVD = as.factor(CVD),
         WHF = as.factor(WHF),
         DIG = as.factor(DIG),
         HOSP = as.factor(HOSP),
         HOSPDAYS = as.integer(HOSPDAYS),
         DEATH = as.factor(DEATH),
         DEATHDAY = as.integer(DEATHDAY))

# Label the factor variables in the data according to the code book
# and add months from randomisation to death or last follow-up
dig.df <- dig.df %>% 
  mutate(TRTMT = factor(TRTMT, levels = c(0,1), labels =c("Placebo", "Treatment")),
         SEX = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
         HYPERTEN = factor(HYPERTEN, levels = c(0,1), labels = c("No", "Yes")),
         CVD = factor(CVD, levels = c(0,1), labels = c("No", "Yes")),
         WHF = factor(WHF, levels = c(0,1), labels = c("No", "Yes")),
         DIG = factor(DIG, levels = c(0,1), labels = c("No", "Yes")),
         HOSP = factor(HOSP, levels = c(0,1), labels = c("No", "Yes")),
         DEATH = factor(DEATH, levels = c(0,1), labels = c("Alive", "Death"))) %>%
  mutate(Month = round(DEATHDAY/30))

### UI 

ui <- dashboardPage(
  skin = "black",
  
  # HEADER ---------------------------------------------------------------------
  
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
        strong("ABOUT Project"),
        height = 40,
        href = "https://github.com/CeirePlace/hds5105_shiny_app.git",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR --------------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", icon = icon("circle-info"),
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
          max(dig.df$AGE, na.rm=TRUE),
          step = 1,
          sep = ""
          ),
        # Need to add ethnicity info to our dataset
        checkboxGroupInput(
          inputId = "Ethnicity",
          label = "Ethnicity",
          choices = c("Aaaaaaaaaa", "Bbbbbbbb", "Cccccccc"), #unique(dig.df$ethnicity)
          selected = c("Aaaaaaaaaa", "Bbbbbbbb", "Cccccccc") #unique(dig.df$ethnicity)
        )
      ),
      menuItem("Medical History", icon = icon("hospital"),
        checkboxGroupInput(
          "xInput",
          label = "Input 1",
          choices = "",
          selected = "",
          inline = TRUE)
      )
    )
  ),
  
  # BODY -----------------------------------------------------------------------
  dashboardBody(
    # Add buttons for PATIENTS & OUTCOMES at the top of the page
    fluidRow(
      column(
        width = 12,
          bsButton("patients", 
                   label = "PATIENTS", 
                   icon = icon("user"), 
                   style = "success"),
          bsButton("Outcomes", 
                   label = "OUTCOMES", 
                   icon = icon("ambulance"), 
                   style = "success")
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
    # PATIENTS            
                tabPanelBody("patients_view", 
      # ADD all code for patient plots here instead of "ADD PATIENT PLOTS HERE"
      "ADD PATIENT PLOTS HERE"),
      
      #OUTCOMES
                tabPanelBody("outcome_view",
      fluidRow(
        box(
          title = "Risk of death within each month of follow-up", status = "danger", solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("plot1", height = 250)
        )
        ),
      )
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  # Filter dataset
  data <- reactive({
    dig.df %>%
      filter(
        SEX %in% input$sexInput,
        AGE >=input$ageInput[1],
        AGE <=input$ageInput[2],
      )
  })
  # Switch to PATIENTS page when patients button is clicked
  observeEvent(input$patients, {
    updateTabsetPanel(session, "view", selected = "patients_view")
    
  })
  # Switch to OUTCOMES page when patients button is clicked  
  observeEvent(input$Outcomes, {
    updateTabsetPanel(session, "view", selected = "outcome_view")
  })
  
  # Survival model (patients at risk and deaths each month)
  surv_df <- reactive({
    surv <- survfit(Surv(Month, DEATH == "Death")~1, data = data())
    surv_summary <- summary(surv, times = 0:59)
    data.frame(Time_months = surv_summary$time, 
               Number_at_risk = surv_summary$n.risk, 
               Number_of_Deaths = surv_summary$n.event)
  })
  
  # Monthly risk of death = deaths in that month / patients at risk at the start of the month
  surv.df <- reactive({
    surv_df() %>% mutate(Monthly_risk = Number_of_Deaths/Number_at_risk)
  })
  
  # Number of patients Box
  output$n_box <- renderValueBox({
    valueBox(
      value = nrow(data()), 
      subtitle = "Patients", 
      icon = icon("users"), 
      color = "light-blue")
  })
  
  # Number of patients & percentage died Box
  output$death_box <- renderValueBox({
    died_pct <- round(mean(data()$DEATH == "Death", na.rm=TRUE)*100,1)
    n_died <- sum(data()$DEATH == "Death", na.rm=TRUE)
  valueBox(
    value = paste0(died_pct, "%"),
    subtitle = paste0("Died (", n_died, ")"),
    icon = icon("heart-pulse"), 
    color = "red")
  })
  
  # Number of patients & percentage hospitalised (all-causes) Box
  output$hosp_box <- renderValueBox({
    hosp_pct <- round(mean(data()$HOSP == "Yes", na.rm=TRUE)*100,1)
    n_hosp <- sum(data()$HOSP == "Yes", na.rm=TRUE)
    valueBox(
      value = paste0(hosp_pct, "%"),
      subtitle = paste0("Hospitalised(", n_hosp, ")"),
      icon = icon("hospital"), 
      color = "yellow")
  })
  
  # Monthly risk of death plot
  output$plot1 <- renderPlotly({
    p1 <- ggplot(surv.df(), aes(x=Time_months, y=Monthly_risk)) + 
      geom_line(colour="darkred") + geom_point(colour="darkred") +
      labs(x = "Time (months)",
           y = "Monthly risk of death") +
      theme_bw()
    ggplotly(p1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

