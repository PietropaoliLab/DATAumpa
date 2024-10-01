library(shiny)

shinyUI(fluidPage(
  titlePanel("DATAumpa!"),
  
  sidebarLayout(
    sidebarPanel(
      # Sidebar per il primo tab (Caricamento File)
      conditionalPanel(
        condition = "input.tabs == 'Caricamento File'",
        fileInput("file", "Carica il dataset (xlsx)", accept = ".xlsx")
      ),
      
      # Sidebar per Tabella 1
      conditionalPanel(
        condition = "input.tabs == 'Tabella 1'",
        uiOutput("var_select_ui"),
        uiOutput("group_select_ui")
      ),
      
      # Sidebar per Regressione Logistica
      conditionalPanel(
        condition = "input.tabs == 'Regressione Logistica'",
        uiOutput("varOutcomeUI"),
        uiOutput("refLevelUI"),
        uiOutput("varPredictorsUI")
      ),
      
      # Sidebar per Regressione Lineare
      conditionalPanel(
        condition = "input.tabs == 'Regressione Lineare'",
        uiOutput("varOutcomeLinUI"),
        uiOutput("varPredictorsLinUI")
      ),
      
      # Sidebar per Box Plot
      conditionalPanel(
        condition = "input.tabs == 'Box Plot'",
        selectInput("xvar", "Variabile Asse X:", choices = NULL),
        selectInput("yvar", "Variabile Asse Y:", choices = NULL),
        checkboxInput("jitter", "Aggiungi Jitter Points", value = FALSE),
        checkboxInput("pvalue", "Calcola overall P-value", value = FALSE),
        checkboxInput("comparisons", "Comparazioni multiple", value = FALSE),
        
        selectInput("palette", "Palette Colori:", choices = c("Accent", "Blues", "BrBG", "BuGn", "BuPu", "Dark2", "GnBu", "Greens", "Greys", "Oranges", 
                                                              "OrRd", "Paired", "Pastel1", "Pastel2", "PiYG", "PRGn", "PuBu", "PuBuGn", "PuOr", "PuRd", 
                                                              "Purples", "RdBu", "RdGy", "RdPu", "RdYlBu", "RdYlGn", "Reds", "Set1", "Set2", "Set3", 
                                                              "Spectral", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")),
        numericInput("width", "Larghezza dei Box Plot:", value = 0.7, min = 0.1, max = 1),
        selectInput("facet_var", "Variabile Facet:", choices = c(".")),
        textInput("xlab", "Nome Asse X:", value = ""),
        textInput("ylab", "Nome Asse Y:", value = ""),
        selectInput("legend_pos", "Posizione Legenda:", choices = c("none", "bottom", "top", "left", "right")),
        selectInput("theme", "Tema:", choices = c("default", "classic", "dark", "light")),
        numericInput("ylim_min", "Minimo Limite Asse Y:", value = NULL),
        numericInput("ylim_max", "Massimo Limite Asse Y:", value = NULL)
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",  # Aggiunto id per i conditionalPanel
                  tabPanel("Caricamento File",
                           h2("Welcome on DATAumpa!"),
                           p(strong("DATAumpa"), "is a web application built with Shiny to simplify data analysis tasks for your lab collaborators (and for your wife!). The name ", strong("DATAumpa")," playfully references the song ", a(href = 'https://youtu.be/odDe4__l7iY', target = '_blank', "Da da um pa by the Kessler Twins"), ", adding a touch of fun to your data work."),
                           br(),
                           
                           # Add Zenodo DOI here
                           p("Feel free to cite ", strong("DATAumpa"), " through its DOI:"),
                           p(HTML('<a href="https://zenodo.org/doi/10.5281/zenodo.13732371"><img src="https://zenodo.org/badge/854073983.svg" alt="DOI"></a>')),
                           br(),
                           
                           h3("Features:"),
                           
                           h4("1. Dynamic Table One"),
                           p("Easily create and customize a summary table using the powerful" ,em("Arsenal "), "library. This feature allows you to select variables, choose grouping options, and generate comprehensive tables with just a few clicks."),
                           br(),

                           
                           h4("2. Multivariate Logistic Regression"),
                           p("Perform logistic regression analysis effortlessly. Select your outcome variable and predictors, and get detailed output including odds ratios, confidence intervals, and p-values. Plus, keep an eye on collinearity with VIF (Variance Inflation Factor) calculations."),
                           br(),
                           
                           h4("3. Multivariate Linear Regression"),
                           p("Run linear regression analyses with ease. Customize your model, and receive detailed outputs, including beta coefficients, confidence intervals, and VIF values to assess collinearity."),
                           br(),
                           
                           h4("4. Custom Box Plot"),
                           p("Create visually appealing box plots to explore your data. Customize the appearance, add jitter points, calculate p-values, and even facet by a grouping variable. Download your final plot as a PDF for easy sharing."),
                           br(),

                           
                           h3("How to Use DATAumpa"),
                           p(""),
                           p(strong("1. Upload Your Dataset:"),  "Start by uploading an Excel file (.xlsx) in the Caricamento File tab."),
                           p(strong("2. Navigate Through Tabs:"),  " Explore the various features by selecting different tabs, including Table One, Logistic Regression, Linear Regression, and Box Plot."),
                           p(strong("3. Customize Outputs:"),  " Use the sidebar to customize variables, model settings, and plot appearance."),
                           p(strong("4. Download Results:"),  " Download your customized box plot or view tables and regression results directly in the app."),
                           br(),
                           br()
                           
                           
                  ),
                  
                  tabPanel("Tabella 1",
                           h3("Dynamic Arsenal Table One in Shiny"),
                           p("Explore more about the Arsenal library at:"),
                           a(href = 'https://mayoverse.github.io/arsenal/index.html', 
                             target = '_blank', "Arsenal Documentation"),
                           uiOutput("tableone")
                  ),
                  
                  tabPanel("Regressione Logistica",
                           h3("Regressione Logistica Multivariata"),
                           actionButton("run", "Esegui Regressione"),
                           h3("Risultati della Regressione Logistica"),
                           tableOutput("resultTable"),
                           h3("Valori di VIF (Variance Inflation Factor)"),
                           tableOutput("vifTable"),
                           p("In generale, un VIF superiore a 5 o 10 può indicare collinearità problematiche.")
                  ),
                  
                  tabPanel("Regressione Lineare",
                           h3("Regressione Lineare Multivariata"),
                           actionButton("runLin", "Esegui Regressione Lineare"),
                           h3("Risultati della Regressione Lineare"),
                           tableOutput("resultTableLin"),
                           h3("Valori di VIF (Variance Inflation Factor)"),
                           tableOutput("vifTableLin"),
                           p("In generale, un VIF superiore a 5 o 10 può indicare collinearità problematiche.")
                  ),
                  
                  tabPanel("Box Plot",
                           h3("Crea un Box Plot Personalizzato"),
                           plotOutput("boxplot"),
                           downloadButton("downloadPlot", "Scarica Box Plot in PDF")
                  )
      )
    )
  )
))