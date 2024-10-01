library(shiny)
library(readxl)
library(arsenal)   # Per la tabella 1
library(broom)     # Per la regressione logistica
library(car)       # Per il VIF
library(dplyr)
library(ggplot2)   # Per il box plot
library(ggpubr)
library(RColorBrewer)


shinyServer(function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  dataset <- reactive({
    req(input$file)  # Ensure file is uploaded
    file <- input$file$datapath
    data <- tryCatch({
      read_excel(file)
    }, error = function(e) {
      NULL
    })
    return(data)
  })
  
  ################### Sezione Tabella 1 ###################
  
  # UI dinamica per la selezione delle variabili
  output$var_select_ui <- renderUI({
    data <- dataset()
    req(data)
    
    selectInput("vars", "Select Variables to Include:",
                choices = names(data), multiple = TRUE)
  })
  
  # UI dinamica per la selezione della variabile di gruppo
  output$group_select_ui <- renderUI({
    data <- dataset()
    req(data)
    
    selectInput("group", "Select Grouping Variable:",
                choices = c("None", names(data)),
                selected = "None")
  })
  
  # Renderizzazione della tabella Arsenal basata sugli input dell'utente
  output$tableone <- renderTable({
    data <- dataset()
    req(data)
    
    selected_vars <- input$vars
    group_var <- input$group
    
    # Verifica che le variabili siano selezionate
    if (length(selected_vars) == 0) {
      return(data.frame("Error" = "No variables selected"))
    }
    
    # Verifica che la variabile di gruppo sia valida
    if (group_var != "None" && !(group_var %in% names(data))) {
      return(data.frame("Error" = paste("Invalid grouping variable:", group_var)))
    }
    
    # Creazione della tabella Arsenal
    if (group_var != "None") {
      tableone <- tableby(as.formula(paste(group_var, "~", paste(selected_vars, collapse = "+"))), data = data)
    } else {
      tableone <- tableby(as.formula(paste("~", paste(selected_vars, collapse = "+"))), data = data)
    }
    
    # Conversione della tabella Arsenal in formato data frame per la visualizzazione
    tableone_df <- summary(tableone, text = "html", pfootnote = TRUE)
    
    # Formattazione e ritorno della tabella
    HTML(gsub("</sup>", ")", gsub("<sup>", " (", capture.output(tableone_df))))
  },
  sanitize.text.function = function(x) x)  # Evita la sanitizzazione del testo durante la renderizzazione
  
  
  
  
  ################### Sezione Regressione Logistica ###################
  
  # UI per la selezione della variabile outcome (Y)
  output$varOutcomeUI <- renderUI({
    req(dataset())
    selectInput("outcome", "Seleziona la variabile outcome (Y)", choices = names(dataset()))
  })
  
  # UI per la selezione dei predittori (X)
  output$varPredictorsUI <- renderUI({
    req(input$outcome)
    checkboxGroupInput("predictors", "Seleziona i predittori (X)", choices = setdiff(names(dataset()), input$outcome))
  })
  
  # UI per la selezione del livello di riferimento della variabile outcome (Y)
  output$refLevelUI <- renderUI({
    req(input$outcome)
    outcome_var <- dataset()[[input$outcome]]
    if (is.factor(outcome_var) || is.character(outcome_var)) {
      selectInput("refLevel", "Seleziona il livello di riferimento per Y", choices = levels(as.factor(outcome_var)))
    }
  })
  
  # Esecuzione della regressione logistica
  results <- eventReactive(input$run, {
    req(input$outcome, input$predictors, input$refLevel)
    
    data <- dataset()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Modifica del livello di riferimento della variabile outcome
    data[[input$outcome]] <- relevel(as.factor(data[[input$outcome]]), ref = input$refLevel)
    
    # Formula per la regressione logistica
    formula <- as.formula(paste(input$outcome, "~", paste(input$predictors, collapse = " + ")))
    
    # Creazione del modello
    model <- glm(formula, data = data, family = binomial())
    
    # Calcolo del VIF
    vif_values <- vif(model)
    
    # Estrazione dei risultati (OR, 95%CI, P value)
    tidy_model <- tidy(model)
    conf_int <- confint(model)
    
    results <- cbind(tidy_model, conf_int)
    results$OR <- exp(results$estimate)
    results$`95% CI Lower` <- exp(results$`2.5 %`)
    results$`95% CI Upper` <- exp(results$`97.5 %`)
    
    results <- results[, c("term", "OR", "95% CI Lower", "95% CI Upper", "p.value")]
    
    # Formattare il p-value con 4 cifre decimali
    results$p.value <- formatC(results$p.value, format = "f", digits = 4)
    
    return(list(results = results, vif_values = vif_values))
  })
  
  # Visualizzazione dei risultati della regressione
  output$resultTable <- renderTable({
    req(results())
    results()$results
  })
  
  # Visualizzazione dei valori di VIF
  output$vifTable <- renderTable({
    req(results())
    vif_data <- as.data.frame(results()$vif_values)
    vif_data$term <- rownames(vif_data)
    colnames(vif_data) <- c("VIF", "Variabile")
    vif_data <- vif_data[, c("Variabile", "VIF")]
    vif_data
  })
  
  
  
  ################### Sezione Regressione Lineare ###################
  
  # UI per la selezione della variabile outcome (Y) per la regressione lineare
  output$varOutcomeLinUI <- renderUI({
    req(dataset())
    selectInput("outcomeLin", "Seleziona la variabile outcome (Y)", choices = names(dataset()))
  })
  
  # UI per la selezione dei predittori (X) per la regressione lineare
  output$varPredictorsLinUI <- renderUI({
    req(input$outcomeLin)
    checkboxGroupInput("predictorsLin", "Seleziona i predittori (X)", choices = setdiff(names(dataset()), input$outcomeLin))
  })
  
  # Esecuzione della regressione lineare
  resultsLin <- eventReactive(input$runLin, {
    req(input$outcomeLin, input$predictorsLin)
    
    data <- dataset()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Formula per la regressione lineare
    formulaLin <- as.formula(paste(input$outcomeLin, "~", paste(input$predictorsLin, collapse = "+")))
    
    # Modello di regressione lineare
    modelLin <- glm(formulaLin, data = data)
    
    # Estrazione dei risultati
    tidy_modelLin <- tidy(modelLin)
    conf_intLin <- confint(modelLin)
    
    resultsLin <- cbind(tidy_modelLin, conf_intLin)
    resultsLin$Beta <- resultsLin$estimate
    resultsLin$`95% CI Lower` <- resultsLin$`2.5 %`
    resultsLin$`95% CI Upper` <- resultsLin$`97.5 %`
    
    resultsLin <- resultsLin[, c("term", "Beta", "95% CI Lower", "95% CI Upper", "p.value")]
    
    # Formattare il p-value con 4 cifre decimali
    resultsLin$p.value <- formatC(resultsLin$p.value, format = "f", digits = 4)
    
    # Calcolo del VIF
    vif_valuesLin <- vif(modelLin)
    
    return(list(resultsLin = resultsLin, vif_valuesLin = vif_valuesLin))
  })

  # Visualizzazione dei risultati della regressione
  output$resultTableLin <- renderTable({
    req(resultsLin())
    resultsLin()$resultsLin
  })
  
  # Renderizzazione della tabella del VIF per la regressione lineare
  output$vifTableLin <- renderTable({
    req(resultsLin())
    vif_dfLin <- as.data.frame(resultsLin()$vif_valuesLin)
    vif_dfLin$term <- rownames(vif_dfLin)
    colnames(vif_dfLin) <- c("VIF", "Variabile")
    vif_dfLin <- vif_dfLin[, c("Variabile", "VIF")]
    vif_dfLin
    
    
        vif_dfLin
  })
  
  
  
  
  ################### Sezione Box Plot ###################
  
  # Aggiornamento dinamico delle scelte per le variabili X e Y
  observe({
    data <- dataset()
    req(data)
    updateSelectInput(session, "xvar", choices = c("Select x variable" = NA, names(data)))
    updateSelectInput(session, "yvar", choices = c("Select y variable" = NA, names(data)))
    updateSelectInput(session, "facet_var", choices = c("None" = ".", names(data)))
  })
  
  # Funzione per generare il boxplot
  generateBoxPlot <- reactive({
    req(input$xvar, input$yvar)
    data <- dataset()
    
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar, fill = input$xvar)) +
      geom_boxplot(width = input$width, outlier.shape = NA) +
      scale_fill_brewer(palette = input$palette) +
      labs(x = input$xlab, y = input$ylab)
    
    if (input$jitter) {
      p <- p + geom_jitter(width = 0.2, size = 1.8, alpha = 0.5, shape = 21, color = "black")
    }
    
    if (input$pvalue) {
      p <- p + stat_compare_means()
    }
    
    if (input$comparisons) {
      # Ensure there are at least two unique values in xvar before attempting to generate comparisons
      unique_xvar <- unique(data[[input$xvar]])
      
      if (length(unique_xvar) >= 2) {
        MyComparisons <- combn(unique_xvar, 2)
        MyComparisons <- split(MyComparisons, col(MyComparisons))
        p <- p + stat_compare_means(comparisons = MyComparisons, method = " wilcox.test")
      } else {
        # Optionally, handle the case where there are not enough groups to compare
        showNotification("Not enough unique groups in the selected x variable for comparisons.", type = "warning")
      }
    }
    
    if (input$facet_var != ".") {
      p <- p + facet_wrap(as.formula(paste('~', input$facet_var)))
    }
    
        if (input$facet_var != ".") {
      p <- p + facet_wrap(as.formula(paste('~', input$facet_var)))
    }
    
    if (!is.null(input$ylim_min) && !is.null(input$ylim_max) &&
        is.numeric(input$ylim_min) && is.numeric(input$ylim_max)) {
      p <- p + ylim(input$ylim_min, input$ylim_max)
    }
    
    if (input$theme == "default") {
      p <- p + theme_bw(base_size = 14) +
        theme(axis.text = element_text(colour = "black", size = 12),
              panel.grid = element_blank(),
              legend.key = element_blank(),
              legend.background = element_blank(),
              strip.background = element_rect(fill = NA, colour = NA),
              text = element_text(family = "sans"))
    } else if (input$theme == "classic") {
      p <- p + theme_classic()
    } else if (input$theme == "dark") {
      p <- p + theme_dark()
    } else if (input$theme == "light") {
      p <- p + theme_light()
    }
    
    p <- p + theme(legend.position = input$legend_pos)
    p
  })
  
  # Render del plot
  output$boxplot <- renderPlot({
    generateBoxPlot()
  })
  
  # Download del box plot in formato PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("boxplot-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = generateBoxPlot(), device = "pdf", width = 8, height = 6)
    }
  )
})