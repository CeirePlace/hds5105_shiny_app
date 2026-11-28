# Install packages if required
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if(!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("survival")) install.packages("survival")
if (!require("survminer")) install.packages("survminer")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("forcats")) install.packages("forcats")
if (!require("shinyjs")) install.packages("shinyjs")

# Load packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(survival)
library(survminer)
library(shinyWidgets)
library(forcats)
library(shinyjs)

# Data 
dig.df <-read.csv("data/DIG.csv")

# Clean like in assignment 4 (option to add other variables)

# Select columns and convert to relevant datatype
dig.df <- dig.df %>% 
  select("ID", "TRTMT", "AGE", "RACE", "SEX", "BMI", 
         "KLEVEL", "CREAT", "DIABP", "SYSBP",
         "PREVMI", "ANGINA", "DIABETES",
         "HYPERTEN", "DIGUSE", "DIURETK", "DIURET", 
         "KSUPP", "ACEINHIB", "NITRATES", "HYDRAL", 
         "VASOD", "DIGDOSE", "CVD", "WHF", "DIG", 
         "HOSP", "HOSPDAYS", "DEATH", "DEATHDAY", 
         "CVDDAYS", "WHFDAYS", "REASON") %>%
  mutate(ID = as.character(ID),
         TRTMT = as.factor(TRTMT),
         AGE = as.integer(AGE),
         RACE = as.factor(RACE),
         SEX = as.factor(SEX),
         BMI = as.numeric(BMI),
         KLEVEL = as.numeric(KLEVEL),
         CREAT = as.numeric(CREAT),
         DIABP = as.numeric(DIABP),
         SYSBP = as.numeric(SYSBP),
         PREVMI = as.factor(PREVMI),
         ANGINA = as.factor(ANGINA),
         DIABETES = as.factor(DIABETES),
         HYPERTEN = as.factor(HYPERTEN),
         DIGUSE = as.factor(DIGUSE),
         DIURETK = as.factor(DIURETK),
         DIURET = as.factor(DIURET),
         KSUPP = as.factor(KSUPP),
         ACEINHIB = as.factor(ACEINHIB),
         NITRATES = as.factor(NITRATES),
         HYDRAL = as.factor(HYDRAL),
         VASOD = as.factor(VASOD),
         DIGDOSE = as.numeric(DIGDOSE),
         CVD = as.factor(CVD),
         WHF = as.factor(WHF),
         DIG = as.factor(DIG),
         HOSP = as.factor(HOSP),
         HOSPDAYS = as.integer(HOSPDAYS),
         DEATH = as.factor(DEATH),
         DEATHDAY = as.integer(DEATHDAY),
         CVDDAYS = as.integer(CVDDAYS),
         WHFDAYS = as.integer(WHFDAYS),
         REASON = as.factor(REASON)) %>%
  mutate(DEATHREASON = fct_na_value_to_level(REASON, level = "6"))

# Label the factor variables in the data according to the code book
# and add months from randomisation to death or last follow-up
dig.df <- dig.df %>% 
  mutate(TRTMT = factor(TRTMT, levels = c(0,1), labels =c("Placebo", "Treatment")),
         RACE = factor(RACE, levels = c(1,2), labels = c("White", "Other")),
         SEX = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
         PREVMI = factor(PREVMI, levels = c(0,1), labels = c("No", "Yes")),
         ANGINA = factor(ANGINA, levels = c(0,1), labels = c("No", "Yes")),
         DIABETES = factor(DIABETES, levels = c(0,1), labels = c("No", "Yes")),
         HYPERTEN = factor(HYPERTEN, levels = c(0,1), labels = c("No", "Yes")),
         DIGUSE = factor(DIGUSE, levels = c(0,1), labels = c("No", "Yes")),
         DIURETK = factor(DIURETK, levels = c(0,1), labels = c("No", "Yes")),
         DIURET = factor(DIURET, levels = c(0,1), labels = c("No", "Yes")),
         KSUPP = factor(KSUPP, levels = c(0,1), labels = c("No", "Yes")),
         ACEINHIB = factor(ACEINHIB, levels = c(0,1), labels = c("No", "Yes")),
         NITRATES = factor(NITRATES, levels = c(0,1), labels = c("No", "Yes")),
         HYDRAL = factor(HYDRAL, levels = c(0,1), labels = c("No", "Yes")),
         VASOD = factor(VASOD, levels = c(0,1), labels = c("No", "Yes")),
         CVD = factor(CVD, levels = c(0,1), labels = c("No", "Yes")),
         WHF = factor(WHF, levels = c(0,1), labels = c("No", "Yes")),
         DIG = factor(DIG, levels = c(0,1), labels = c("No", "Yes")),
         HOSP = factor(HOSP, levels = c(0,1), labels = c("No", "Yes")),
         DEATH = factor(DEATH, levels = c(0,1), labels = c("No", "Yes")),
         DEATHREASON = factor(DEATHREASON, levels = c(1:6), labels = c("Worsening Heart Failure", "Other Cardiac", "Other Vascular", "Unknown", "Non cardiac, nonvascular cause", "Alive"))) %>% # Death = Yes, Alive = No
  mutate(DEATH_month = round(DEATHDAY/30), CVD_month = round(CVDDAYS/30), 
         WHF_month = round(WHFDAYS/30))

colour1<- "#A2CD5A"
colour2<- "#CD661D"
colour3<- "#333333"