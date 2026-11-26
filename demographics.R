# UI ---------------------------------------------------------------        
#Sidebar menu item containing interactive demographic filters
demographics_menu <- menuItem("Demographics",
                              icon = icon("circle-info"),
                              #Sex checkboxes
                              checkboxGroupInput(
                                "sexInput",
                                label = "Sex",
                                choices = unique(dig.df$SEX),
                                selected = unique(dig.df$SEX),
                                inline = TRUE
                                ),
                              #Treatment Arm checkboxes
                              checkboxGroupInput(
                                "TrtmtArmInput",
                                label = "Treatment Arm",
                                choices = levels(dig.df$TRTMT),
                                selected = levels(dig.df$TRTMT),
                                inline = TRUE
                                ),
                              #Slider for Age
                              sliderInput(
                                "ageInput", 
                                label = "Age",
                                value = c(min(dig.df$AGE, na.rm = TRUE), max(dig.df$AGE, na.rm=TRUE)),
                                min = min(dig.df$AGE, na.rm = TRUE),
                                max = max(dig.df$AGE, na.rm=TRUE),
                                step = 1,
                                sep = ""
                                ),
                              #Ethnicity checkboxes
                              checkboxGroupInput(
                                inputId = "Ethnicity",
                                label = "Ethnicity",
                                choices = levels(dig.df$RACE),
                                selected = levels(dig.df$RACE) 
                                ),
                              #Slider for BMI
                              sliderInput(
                                "bmiInput",
                                label = "Body Mass Index (BMI)",
                                value = c(min(dig.df$BMI, na.rm = TRUE), max(dig.df$BMI, na.rm = TRUE)),
                                min = min(dig.df$BMI, na.rm = TRUE),
                                max = max(dig.df$BMI, na.rm = TRUE),
                                step = 1,
                                sep = ""
                                ),
                              #Slider for SYSBP
                              sliderInput(
                                "sysBPInput",
                                label = "Systolic Blood Pressure (SBP)",
                                value = c(min(dig.df$SYSBP, na.rm = TRUE), max(dig.df$SYSBP, na.rm = TRUE)),
                                min = min(dig.df$SYSBP, na.rm = TRUE),
                                max = max(dig.df$SYSBP, na.rm = TRUE),
                                step = 1,
                                sep = ""
                                ),
                              #Slider for DIABP
                              sliderInput(
                                "diaBPInput",
                                label = "Diastolic Blood Pressure (DBP)",
                                value = c(min(dig.df$DIABP, na.rm = TRUE), max(dig.df$DIABP, na.rm = TRUE)),
                                min = min(dig.df$DIABP, na.rm = TRUE),
                                max = max(dig.df$DIABP, na.rm = TRUE),
                                step = 1,
                                sep = ""
                                )
                              )
#Variable labels for interactive dropdown plots
demographic_choices <- c("Age" = "AGE",
                         "Sex" = "SEX",
                         "Body Mass Index (BMI)" = "BMI",
                         "Systolic Blood Pressure (SBP)" = "SYSBP",
                         "Diastolic Blood Pressure (DBP)" = "DIABP")

#Patients panel body
patients_view <- tabPanelBody("patients_view",
                              #Dropdown to choose demographic feature to plot
                              selectInput("patient_demographics",
                                          "Select Feature",
                                          choices = demographic_choices),
                              #Row with boxes for interactive plots
                              fluidRow(
                                box(title = textOutput("demo_plot_title", inline = TRUE), 
                                    status = "primary", 
                                    solidHeader = TRUE,
                                    collapsible = TRUE, 
                                    width = 6,
                                    plotlyOutput("plot_demographic", height = 350)),
                                box(title = textOutput("violin_plot_title", inline = TRUE), 
                                    status = "primary", 
                                    solidHeader = TRUE,
                                    collapsible = TRUE, 
                                    width = 6,
                                    plotlyOutput("plot_violin", height = 350)),
                                )
                              )

# SERVER -----------------------------------------------------
demographics_server <- function(input, output, session, data) {
  
  #Generate box title based on demographic feature selected (for histogram/barchart)
  output$demo_plot_title <- renderText({
    #Wait until a feature has been selected
    req(input$patient_demographics)
    #Use display label whose value matches the selected feature name
    var_label <- names(demographic_choices)[demographic_choices == input$patient_demographics]
    paste(var_label, "Distribution by Treatment Group")
    })
  
  #Generate box title based on demographic feature selected (for violin plot/barchart)
  output$violin_plot_title <- renderText({
    req(input$patient_demographics)
    var_label <- names(demographic_choices)[demographic_choices == input$patient_demographics]
    if(input$patient_demographics == "SEX"){
      "Sex Composition within each treatment group (%)"
    }
    else{
      paste(var_label, "Distribution by Treatment Group")
    }
  })
  
  ## First plot ---------------------------------------------------------
  #Interactive plot of the selected demographic feature, split by treatment arm
  output$plot_demographic <- renderPlotly({
    req(input$patient_demographics)
  
    #Selected column/feature and its label
    var <- input$patient_demographics
    var_label <- names(demographic_choices)[demographic_choices == var]
    
    #Drop rows with missing values in the selected variable
    plot_data <- data() %>%
      # .data[[var]] selects the column from the data by its string name
      filter(!is.na(.data[[var]]))
    
    #Generating counts for SEX barchart
    plot_counts <- plot_data %>%
      count(.data[[var]], TRTMT) %>%
      #Formatting hover text for plot
      mutate(hover_text = sprintf("%s: %s<br>Treatment: %s<br>Participants: %d", var_label, .data[[var]], TRTMT, n))
    
    #Plot for continuous variables
    plt <- ggplot(plot_data, aes(x = .data[[var]], fill = TRTMT)) +
      scale_x_continuous(n.breaks = 12) +
      scale_y_continuous(n.breaks = 5) +
      scale_fill_manual(values = c("Placebo" = colour1,
                                   "Treatment" = colour2)) +
      labs(x = var_label, y = "Number of Participants", fill = "Treatment Arm") +
      theme_bw()
    
    #Plot for categorical variables, if/else statement depending on variable chosen from dropdown
    if (var == "SEX") {
      p <- ggplot(plot_counts, aes(x = .data[[var]], y = n, fill = TRTMT, text = hover_text)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Placebo" = colour1,
                                     "Treatment" = colour2)) +
        labs(x = var_label, y = "Number of Participants", fill = "Treatment Arm") +
        theme_bw()
    } else {
      p <- plt + geom_histogram(position = "identity", alpha = 0.5, bins = 50,
                              aes(text = sprintf("%s ~%.2f<br>Participants: %d<br>Treatment: %s",
                                                 var_label,
                                                 after_stat(x),
                                                 after_stat(count),
                                                 levels(plot_data$TRTMT)[after_stat(group)])))
    }
    #Generate interactive plotly plot with formatted hover text
    ggplotly(p, tooltip = "text")
  })
  
  ##Second Plot ------------------------------------------------------------------
  #Interactive plot of the selected demographic feature, split by treatment arm
  output$plot_violin <- renderPlotly({
    req(input$patient_demographics)
    
    #Selected column/feature and its label
    var <- input$patient_demographics
    var_label <- names(demographic_choices)[demographic_choices == var]
    
    #Drop rows with missing values in the selected variable
    plot_data <- data() %>%
      # .data[[var]] selects the column from the data by its string name
      filter(!is.na(.data[[var]]))
    
    #Plot for categorical variables, if/else statement depending on variable chosen from dropdown
    if (var == "SEX") {
      #Generating treatment arm percentages for SEX barchart
      plot_trtmt_arm_pct <- plot_data %>%
        #Create category column for variable chosen
        mutate(category = .data[[var]]) %>%
        #Generate counts for treatment and the category
        count(TRTMT, category) %>%
        #Group by treatment arm
        group_by(TRTMT) %>%
        #Create column with percentages
        mutate(pct = n / sum(n) * 100,
               #Formatting hover text for plot
               hover_text = sprintf("%s: %s<br>Treatment: %s<br>%.2f%% of arm (n = %d)",
                                    var_label, category, TRTMT, pct, n)) %>%
        ungroup()
      
      p <- ggplot(plot_trtmt_arm_pct, aes(x = category, y = pct, fill = TRTMT, text = hover_text)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Placebo" = colour1,
                                     "Treatment" = colour2)) +
        labs(x = var_label, y = "% of Treatment Group", fill = "Treatment Arm") +
        theme_bw()
      #Generate interactive plotly plot with formatted hover text
      ggplotly(p, tooltip = "text")
    } else {
      p <- ggplot(plot_data, aes(x = TRTMT, y = .data[[var]], fill = TRTMT)) +
        geom_violin(alpha = 0.5) +
        geom_boxplot(width = 0.2, colour = colour3) +
        scale_fill_manual(values = c("Placebo" = colour1,
                            "Treatment" = colour2)) +
        labs(x = "Treatment Group", y = var_label) +
        theme_bw() 
      #Generate interactive plotly plot
      ggplotly(p)
    }
  })
}
