# UI --------------------------------------------------------------- 
outcome_view <- tabPanelBody("outcome_view",
                 fluidRow(
                   box(
                     background = "olive",
                     width = 12,
                     column(width=6,
                            selectInput("patient_outcomes", "Select Patient Event",
                                        c("Death" = "DEATH",
                                          "Cardiovascular Disease Hospitalisation" = "CVD",
                                          "Worsening Heart Failure Hospitalisation" = "WHF"),
                                        width = "100%")),
                     column(width=6,
                            checkboxGroupInput("select_group", "Select Treatment Arm",
                                               choices = c("Placebo", "Treatment"),
                                               selected = c("Placebo", "Treatment"),
                                               inline = TRUE))
                   )
                 ),
                 
                 fluidRow(
                   column(width = 6, uiOutput("plot_event_ui")),
                   column(width = 6, uiOutput("plot_KMcurves_ui"))
                 )
    )

# SERVER ------------------------------------------------
outcomes_server <- function(input, output, session, data) {
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
                         Monthly_risk = Number_of_events/Number_at_risk,
                         hover = paste0(Group,
                                        "<br>Month: ", Time_months,
                                        "<br>Patients at risk: ", Number_at_risk,
                                        "<br>Events: ", Number_of_events,
                                        "<br>Monthly risk: ", round(Monthly_risk, 4)))
    })
  

  
  # Event Table 2 (Comparison)
  event_table <- reactive({
    req(input$select_group)
    validate(need(length(input$select_group) == 2,
                  "Select both Placebo and Treatment to compare the groups."))
    event <- input$patient_outcomes
    table1 <- data() %>% filter(TRTMT %in% input$select_group) %>%
      group_by(TRTMT) %>%
      summarise(Patients=n(), 
                Events = sum(.data[[event]] == "Yes")) %>%
      mutate(`% with event` = paste0(round((Events/Patients)*100,1), "%")) %>%
      rename(Treatment = TRTMT)
    
    # Risk Difference
    df <- table1 %>% mutate(prop_event = Events/Patients)
    
    rd <- df$prop_event[2] - df$prop_event[1]
    rd_test <- prop.test(x = df$Events[c(2, 1)],
                              n = df$Patients[c(2, 1)])
    rd_ci <- rd_test$conf.int

    # Table2 comparison table
    table2 <- data.frame(Measure = c("Risk Difference", "Chi-squared p-value"),
                                    Estimate = c(rd, rd_test$p.value),
                                    "Lower 95% CI" = c(rd_ci[1], NA),
                                    "Upper 95% CI" = c(rd_ci[2], NA),
                         check.names = FALSE)
    list(table1 = table1, table2 = table2)
  })
  
  # Kaplan-Meier model: time to event by treatment group (events = death/CVD/WHF)
  km.df <- reactive({
    df <- data() %>% filter(TRTMT %in% input$select_group)
    df$tte <- df[[paste0(input$patient_outcomes, "_month")]]
    df$status <- df[[input$patient_outcomes]] == "Yes"
    km_fit <- surv_fit(Surv(tte, status)~TRTMT, data = df)
    list(fit = km_fit, data = df)
    })
  
  # Cox model: hazard ratio for treatment vs placebo for each outcome
  hr <- reactive({
    validate(need(length(input$select_group) == 2,
                  "Select both Placebo and Treatment to compare the groups."))
    df <- data() %>% filter(TRTMT %in% input$select_group)
    df$tte <- df[[paste0(input$patient_outcomes, "_month")]]
    df$status <- df[[input$patient_outcomes]] == "Yes"
    
    cox_model <- coxph(Surv(tte, status) ~ TRTMT, data = df)
    hr_summary <- summary(cox_model)
    
    hr <- hr_summary$conf.int[1, "exp(coef)"]
    lower_ci  <- hr_summary$conf.int[1, "lower .95"]
    upper_ci <- hr_summary$conf.int[1, "upper .95"]
    p <- hr_summary$coefficients[1, "Pr(>|z|)"]
    
    list(hr = hr, lower_ci = lower_ci, upper_ci = upper_ci, p = p)
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
      tabsetPanel(
        id = "event_tabs",
        selected = isolate(input$event_tabs),
        tabPanel("Plot",
                 withSpinner(plotlyOutput("plot_event", height = 350),
                             type=5, color = "purple", color.background = "white")),
        tabPanel("Table", 
                 h4("Events by treatment group"),
                 tableOutput("table_event1"),
                 h4("Treatment vs. placebo comparison"),
                 tableOutput("table_event2")
                 
      )
      )
      )
    })
  
  # Change plot title for each outcome
  output$plot_event_title <- renderText({
    switch(input$patient_outcomes,
           "DEATH" = "Risk of death by treatment group",
           "CVD" = "Risk of first CVD hospitalisation by treatment group",
           "WHF" = "Risk of first WHF hospitalisation by treatment group"
           )
    })
  
  # Create interactive monthly risk plot
  output$plot_event <- renderPlotly({
    req(input$select_group)
    plot_data <- surv.df() %>% filter(Group %in% input$select_group)
    # Change line colour for each outcome
    line_colour <- switch(input$patient_outcomes,
                          "DEATH" = "tomato",
                          "CVD" = "chocolate",
                          "WHF" = "tan4"
                          )
    
    p1 <- ggplot(plot_data, aes(x=Time_months, y=Monthly_risk, colour = Group)) + 
      geom_line() + geom_point(aes(text = hover)) +
      labs(x = "Time (months)", y = "Monthly risk", colour = "Group") +
      scale_colour_manual(values = c("Placebo" = "darkgrey", "Treatment" = line_colour)) +
      theme_bw()
    ggplotly(p1, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x=0, y=1.1, 
                           xanchor = "left", yanchor = "bottom"))
    })
  
  # Create event tables in Table tab
  # Table 1 - top
  output$table_event1 <- renderTable({
    event_table()$table1
  }, striped = TRUE, bordered = TRUE, hover = TRUE,
  width = "100%", align = "lccc")
  
  # Table 2 - bottom
  output$table_event2 <- renderTable({
    event_table()$table2
  }, striped = TRUE, bordered = TRUE, hover = TRUE,
  width = "100%", align = "lccc", digits = 4, na = "", caption = "Risk difference: Treatment - Placebo")
  
  # Add Kaplan-Meier Plots with same options
  # Kaplan-Meier survival plot with same options
  output$plot_KMcurves_ui <- renderUI({
    plot_status <- if (input$patient_outcomes == "DEATH") "danger" else "warning"
    box(
      title = textOutput("plot_KMcurves_title", inline = TRUE),
      status = plot_status, solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      tabsetPanel(
        id = "event_tabs",
        selected = isolate(input$event_tabs),
        tabPanel("Plot",
                 withSpinner(plotOutput("plot_KMcurves", height = 350), 
                             type=5, color = "purple", color.background = "white")),
        tabPanel("Table", 
                 h4("Treatment vs. placebo comparison"),
                 tableOutput("hr_table"),
                 h5(textOutput("hr_text"))
                 
        )
      )
      )
    })
  # Change plot title for each outcome
  output$plot_KMcurves_title <- renderText({
    switch(input$patient_outcomes,
           "DEATH" = "Time to death by treatment arm",
           "CVD" = "Time to first CVD hospitalisation by treatment arm",
           "WHF" = "Time to first WHF hospitalisation by treatment arm"
           )
    })
  
  # Create KM plot
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
                     legend.title = "Treatment Arm",
                     legend.labs = input$select_group,
                     risk.table.title = "Number at risk",
                     risk.table.y.text = TRUE,
                     risk.table.fontsize = 3.5,
                     tables.theme = theme_bw(),
                     pval.coord = c(0, 0.25))
    print(p2)
    })
  
  # Hazard ratio table
  output$hr_table <- renderTable({
    results <- hr()
    data.frame(
      `Hazard Ratio` = round(results$hr, 2),
      `Lower 95% CI` = round(results$lower_ci, 2),
      `Upper 95% CI` = round(results$upper_ci, 2),
      `p-value` = ifelse(results$p < 0.001, "<0.001", round(results$p, 3)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE,
  width = "100%", align = "lccc", caption = "Hazard ratio from a Cox proportional hazards model")
  
  output$hr_text <- renderText({
    results <- hr()
    event_name <- switch(input$patient_outcomes,
      "DEATH" = "death",
      "CVD" = "cardiovascular disease hospitalisation",
      "WHF" = "worsening heart failure hospitalisation"
    )
    low_high <- if(results$hr < 1) "lower" else "higher"
    pct <- round(abs(1-results$hr) *100)
    signif <- if (results$p < 0.05) "statistically significant" else "not statistically significant"
    
    paste0("Patients on digoxin had a ", pct, "% ", low_high, " hazard of ", 
           event_name, " than placebo (HR ", round(results$hr, 2), " ). This
           difference was ", signif, ".")
  })
}
