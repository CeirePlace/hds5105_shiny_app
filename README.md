# HDS5105 DIG Shiny App

Interactive Shiny dashboard exploring patient characteristics and outcomes from the Digitalis Investigation Group (DIG) trial.


## Features
- Filter patients by demographics, medical history, medication and cause of death
- Compared demographics between treatment arms
- Monthly risk and Kaplan-Meier plots for death, cardiovascular disease and worsening heart failure hospitalisation
- Risk difference and hazard ratio tables


## How to Run
1. Clone this repo
2. Open app.R in Rstudio and click Run App


## Structure
| File | Contents |
|------|----------|
| `app.R` | Loads the other files and launches the app |
| `global.R` | Packages, data loading & cleaning |
| `about.R` | About page |
| `demographics.R` | Sidebar, demographics filters & Patients page |
| `outcomes.R` | Outcomes page |
| `ui_main.R` | main UI |
| `sever_main.R` | main Server |

## Data
- Data comes from `data\DIG.csv` and is available in data folder
- In order to create an anonymous dataset that protects patient confidentiality, most variables have been permuted over the set of patients within treatment group.

## Authors 
- Sara Bartolo 
- Ceire Burke
