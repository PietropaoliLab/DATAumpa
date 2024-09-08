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
      
      # Sidebar per Box Plot
      conditionalPanel(
        condition = "input.tabs == 'Box Plot'",
        selectInput("xvar", "Variabile Asse X:", choices = NULL),
        selectInput("yvar", "Variabile Asse Y:", choices = NULL),
        checkboxInput("jitter", "Aggiungi Jitter Points", value = FALSE),
        checkboxInput("pvalue", "Calcola P-value", value = FALSE),
        
        
        selectInput("palette", "Palette Colori:", choices = c("Set1", "Accent", "Blues")),
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
                           h3("Benvenuti su DATAumpa!"),
                           p("Questa applicazione consente di caricare un dataset in formato Excel (.xlsx) e di eseguire diverse analisi."),
                           p("Usa i tab in alto per navigare tra le diverse funzionalità disponibili."),
                           p("Inizia caricando il tuo file Excel utilizzando il widget nella barra laterale.")
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
                  
                  tabPanel("Box Plot",
                           h3("Crea un Box Plot Personalizzato"),
                           plotOutput("boxplot"),
                            downloadButton("downloadPlot", "Scarica Box Plot in PDF")
                           
                  )
      )
    )
  )
))
