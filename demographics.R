# UI ---------------------------------------------------------------        
##Demographics Menu-------------------------------------------------
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
                              conditionalPanel(
                                condition = "input.view == 'patients_view'",
                                checkboxGroupInput(
                                  "TrtmtArmInput",
                                  label = "Treatment Arm",
                                  choices = levels(dig.df$TRTMT),
                                  selected = levels(dig.df$TRTMT),
                                  inline = TRUE
                                  )),
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
                                selected = levels(dig.df$RACE),
                                inline = TRUE
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

##Medical History Menu --------------------------------------------------
medical_hx_menu <- menuItem("Medical History", 
                           icon = icon("book-medical", lib = "font-awesome"),
                           checkboxGroupInput("diabetesInput",
                                              label = "Diabetes",
                                              choices = levels(dig.df$DIABETES),
                                              selected = levels(dig.df$DIABETES),
                                              inline = TRUE),
                           checkboxGroupInput("htnInput",
                                              label = "Hypertension",
                                              choices = levels(dig.df$HYPERTEN),
                                              selected = levels(dig.df$HYPERTEN),
                                              inline = TRUE),
                           checkboxGroupInput("anginaInput",
                                              label = "Current Angina",
                                              choices = levels(dig.df$ANGINA),
                                              selected = levels(dig.df$ANGINA),
                                              inline = TRUE),
                           checkboxGroupInput("prevmiInput",
                                              label = "Previous Myocardial Infarction",
                                              choices = levels(dig.df$PREVMI),
                                              selected = levels(dig.df$PREVMI),
                                              inline = TRUE)
)

##Medication Menu --------------------------------------------------
medication_menu <- menuItem("Medication", 
                            icon = icon("pills", lib = "font-awesome"),
                            checkboxGroupInput("diguseInput",
                                               label = "Digoxin use within week prior to randomisation",
                                               choices = levels(dig.df$DIGUSE),
                                               selected = levels(dig.df$DIGUSE),
                                               inline = TRUE),
                            checkboxGroupInput("diuretkInput",
                                               label = "Potassium-sparing Diuretic use",
                                               choices = levels(dig.df$DIURETK),
                                               selected = levels(dig.df$DIURETK),
                                               inline = TRUE),
                            checkboxGroupInput("diuretInput",
                                               label = "Other Diuretic use",
                                               choices = levels(dig.df$DIURET),
                                               selected = levels(dig.df$DIURET),
                                               inline = TRUE),
                            checkboxGroupInput("ksuppInput",
                                               label = "Potassium Supplement use",
                                               choices = levels(dig.df$KSUPP),
                                               selected = levels(dig.df$KSUPP),
                                               inline = TRUE),
                            checkboxGroupInput("aceinhibInput",
                                               label = "ACE Inhibitor use",
                                               choices = levels(dig.df$ACEINHIB),
                                               selected = levels(dig.df$ACEINHIB),
                                               inline = TRUE),
                            checkboxGroupInput("nitratesInput",
                                               label = "Nitrate use (oral or paste)",
                                               choices = levels(dig.df$NITRATES),
                                               selected = levels(dig.df$NITRATES),
                                               inline = TRUE),
                            checkboxGroupInput("hydralInput",
                                               label = "Hydralazine use",
                                               choices = levels(dig.df$HYDRAL),
                                               selected = levels(dig.df$HYDRAL),
                                               inline = TRUE),
                            checkboxGroupInput("vasodInput",
                                               label = "Other vasodilator use",
                                               choices = levels(dig.df$VASOD),
                                               selected = levels(dig.df$VASOD),
                                               inline = TRUE),
                            sliderInput("digdoseInput",
                                        label = "Dose of Digoxin Prescribed (mg/day)",
                                        value = c(min(dig.df$DIGDOSE, na.rm = TRUE), max(dig.df$DIGDOSE, na.rm = TRUE)),
                                        min = min(dig.df$DIGDOSE, na.rm = TRUE),
                                        max = max(dig.df$DIGDOSE, na.rm = TRUE),
                                        step = 0.005,
                                        sep = "")
)

##MORTALITY MENU -----------------------------------------------------
mortality_menu <- menuItem("Cause of Death", 
                            icon = icon("heart-crack", lib = "font-awesome"),
                            checkboxGroupInput("deathreasonInput",
                                               label = "Cause of Death",
                                               choices = levels(dig.df$DEATHREASON),
                                               selected = levels(dig.df$DEATHREASON),
                                               inline = FALSE)
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
