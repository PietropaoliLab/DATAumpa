DATAumpa! - Shiny App for Data Exploration and Analysis

This repository contains the code for a Shiny application named DATAumpa! designed to aid in data exploration and analysis.

Features:

File Upload: Load your dataset in Excel (.xlsx) format.
Dynamic Arsenal Table: Explore descriptive statistics of your data interactively.
Logistic Regression: Perform multivariate logistic regression analysis.
Box Plot Customization: Create personalized box plots with various options.
Required Packages:

shiny for building the interactive web application.
readxl for reading Excel files.
arsenal for generating the dynamic Arsenal table.
broom for extracting model results (logistic regression).
car for calculating the Variance Inflation Factor (VIF).
dplyr for data manipulation.
ggplot2 and ggpubr for creating box plots.
File Structure:

ui.R: Defines the user interface elements of the Shiny app.
server.R: Implements the server-side logic and data processing functionalities.
Explanation of Code Sections:

ui.R:

library(shiny): Imports the shiny package for building the web application.
shinyUI Function: Defines the overall layout of the app.
fluidPage creates a full-width page.
titlePanel sets the title of the app ("DATAumpa!").
sidebarLayout creates a sidebar and main panel layout.
Sidebar:
Conditional panels (conditionalPanel) dynamically display content based on the selected tab.
The first tab ("Caricamento File") allows uploading the Excel file.
Subsequent tabs ("Tabella 1", "Regressione Logistica", "Box Plot") offer options specific to their functionalities.
Main Panel:
tabsetPanel defines the navigation tabs for different functionalities.
Each tab panel provides relevant controls and outputs.
server.R:

Required Libraries: Imports necessary packages for data processing and analysis.
dataset Reactive Expression: Reactively reads the uploaded Excel file and stores the data frame.
Tabella 1 Section:
output$var_select_ui and output$group_select_ui define reactive UI elements for selecting variables and the grouping variable for the Arsenal table.
output$tableone renders the Arsenal table based on user selections and data availability.
Regressione Logistica Section:
output$varOutcomeUI, output$varPredictorsUI, and output$refLevelUI define UI elements for selecting the outcome variable, predictors, and reference level (if applicable).
results is an event reactive that executes the logistic regression analysis upon clicking the "Esegui Regressione" button.
It validates user input, prepares the data, formulates the regression model, calculates VIF, and extracts results (OR, CI, p-value).
output$resultTable displays the logistic regression results table.
output$vifTable displays the VIF values for each predictor.
Box Plot Section:
observe updates the available choices for X and Y variables and the facet variable dynamically based on the loaded dataset.
generateBoxPlot reactive function creates the box plot with user-specified options (variables, aesthetics, theme, etc.).
output$boxplot renders the box plot in the main panel.
output$downloadPlot provides a download button for saving the box plot as a PDF file.
How to Use:

Clone this repository to your local machine.
Install the required packages (library(shiny), etc.) in your R environment.
Run the server.R and ui.R files together using runApp(shinyAppDir()).
Access the Shiny app in your web browser (usually at http://127.0.0.1:8000/).
Upload your Excel file and explore the different functionalities offered by DATAumpa!
