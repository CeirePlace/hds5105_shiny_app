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
  
  # Survival model (patients at risk and deaths each month)
  surv_df <- reactive({
    df <- data()
    # Event = Death or CVD hosp or WHF hosp
    event <- input$patient_outcomes
    time_to_event <- paste0(event, "_month")
    surv <- survfit(Surv(df[[time_to_event]], df[[event]] == "Yes")~TRTMT, data = df)
    surv_summary <- summary(surv, times = 0:59)
    data.frame(Group = surv_summary$strata,
               Time_months = surv_summary$time, 
               Number_at_risk = surv_summary$n.risk, 
               Number_of_events = surv_summary$n.event)
  })
  
  # Monthly risk of death = deaths in that month / patients at risk at the start of the month
  surv.df <- reactive({
    surv_df() %>% mutate(Group = factor(Group, levels = c("TRTMT=Placebo", "TRTMT=Treatment"), 
                                        labels = c("Placebo", "Treatment")),
                         Monthly_risk = Number_of_events/Number_at_risk)
  })
  
  # Kaplan-Meier model: time to event by treatment group (events = death/CVD/WHF)
  km.df <- reactive({
    df <- data() %>% filter(TRTMT %in% input$select_group)
    df$tte <- df[[paste0(input$patient_outcomes, "_month")]]
    df$status <- df[[input$patient_outcomes]] == "Yes"
    km_fit <- surv_fit(Surv(tte, status)~TRTMT, data = df)
    list(fit = km_fit, data = df)
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
    died_pct <- round(mean(data()$DEATH == "Yes", na.rm=TRUE)*100,1)
    n_died <- sum(data()$DEATH == "Yes", na.rm=TRUE)
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
  
  # Monthly risk of event plot (event = Death, WHF hosp, CVD hosp)
  # Using render UI so that the box colour can change to match the value box colours for death and hosp
  output$plot_event_ui <- renderUI({
    plot_status <- if (input$patient_outcomes == "DEATH") "danger" else "warning"
    box(
      title = textOutput("plot_event_title", inline = TRUE),
      status = plot_status, solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      plotlyOutput("plot_event", height = 350)
    )
  })
  # Change plot title for each outcome
  output$plot_event_title <- renderText({
    switch(input$patient_outcomes,
           "DEATH" = "Risk of death within each month of follow-up",
           "CVD" = "Risk of first CVD hospitalisation within each month of follow-up",
           "WHF" = "Risk of first WHF hospitalisation within each month of follow-up"
    ) 
  })
  
  # Create interactive plot
  output$plot_event <- renderPlotly({
    req(input$select_group)
    plot_data <- surv.df() %>% filter(Group %in% input$select_group)
    # Change line colour for each outcome
    line_colour <- switch(input$patient_outcomes,
                          "DEATH" = "tomato",
                          "CVD" = "chocolate",
                          "WHF" = "tan4"
    )
    # Change y-axis label for each outcome
    y_label <- switch(input$patient_outcomes,
                      "DEATH" = "Monthly risk of death",
                      "CVD" = "Monthly risk of CVD",
                      "WHF" = "Monthly risk of WHF"
    )
    p1 <- ggplot(plot_data, aes(x=Time_months, y=Monthly_risk, colour = Group)) + 
      geom_line() + geom_point() +
      labs(x = "Time (months)", y = y_label, colour = "Group") +
      scale_colour_manual(values = c("darkgrey", line_colour)) +
      theme_bw()+
      theme(legend.position = "topleft")
    ggplotly(p1) %>%
      layout(legend = list(orientation = "h", x=0, y=1.1, 
                           xanchor = "left", yanchor = "bottom"))
  })
  
  # Add Kaplan-Meier Plots with same options
  # Kaplan-Meier survival plot with same options
  output$plot_KMcurves_ui <- renderUI({
    plot_status <- if (input$patient_outcomes == "DEATH") "danger" else "warning"
    box(
      title = textOutput("plot_KMcurves_title", inline = TRUE),
      status = plot_status, solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      plotOutput("plot_KMcurves", height = 350)
    )
  })
  # Change plot title for each outcome
  output$plot_KMcurves_title <- renderText({
    switch(input$patient_outcomes,
           "DEATH" = "Kaplan-Meier plot: Time to death",
           "CVD" = "Kaplan-Meier plot: Time to first CVD hospitalisation",
           "WHF" = "Kaplan-Meier plot: Time to first WHF hospitalisation"
    ) 
  })
  
  # Create interactive plot
  output$plot_KMcurves <- renderPlot({
    req(input$select_group)
    # Change line colour for each outcome
    line_colour <- switch(input$patient_outcomes,
                          "DEATH" = "tomato",
                          "CVD" = "chocolate",
                          "WHF" = "tan4"
    )
    # Change y-axis label for each outcome
    y_label <- switch(input$patient_outcomes,
                      "DEATH" = "Survival probability",
                      "CVD" = "Probability of no CVD hosp",
                      "WHF" = "Probability of no WHF hosp"
    )
    p2 <- ggsurvplot(km.df()$fit, 
                     data = km.df()$data,
                     xlab = "Months since randomisation",
                     ylab = y_label,
                     palette = unname(c("Placebo" = "darkgrey", "Treatment" = line_colour)[input$select_group]),
                     pval = length(input$select_group) == 2,
                     risk.table = TRUE,
                     break.x.by = 5,
                     xlim = c(0, 60),
                     legend.title = "Group",
                     legend.labs = input$select_group,
                     risk.table.title = "Number at risk",
                     risk.table.y.text = TRUE,
                     risk.table.fontsize = 3.5,
                     tables.theme = theme_bw(),
                     pval.coord = c(0, 0.25))
    print(p2)
  })
  
  ## PATIENTS PLOTS -----------------------------------------------------
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
## RUN APP ------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)