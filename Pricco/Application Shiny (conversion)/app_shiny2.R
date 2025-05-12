# Exemple de dataset

library(shiny)
library(DT)

files_names <- list.files(path = "data")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Calcul du Kg",
                 # Slection du fichier
                 selectInput("select_file", "1. Selectionnez le fichier", choices = files_names, selected = NULL),
                 
                 actionButton("valide_file", "Valider le fichier selectione"),
                 textOutput("file_valide"),
                 # Transformation de valeurs en facteur
                 uiOutput("trans_factor"),
                 
                 actionButton("apply_trans_factor", "Transformer"),
                 # Parti affection valeur pour le facteurs 
                 uiOutput("var_categ"),
                 
                 uiOutput("mod_selector"),
                 
                 
                 numericInput("group_value", "3. Combien de cette unité pour 1kg :", value = 0),
                 actionButton("apply_group", "Associer aux modalités sélectionnées"),
                 uiOutput("quant_var_selector"),
                 uiOutput("price_var_selector")
                 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Choix csv", h4("Jeu de données"),
                 dataTableOutput("file_csv")),
        
        tabPanel("jeu de données filtré",h4("Affectations actuelles :"),
                 tableOutput("valeurs_associees"),
                 dataTableOutput("filtered_with_values"))
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  data_select <- reactiveVal(NULL)
  nom_data <- reactiveVal(NULL)
  
  # Slection du fichier
  
  observeEvent(input$valide_file,{
    req(input$select_file)
    
    file_path <- input$select_file
    
    dt <- read.csv(paste("data/", input$select_file, sep = ""))
    
    data_select(dt)
    nom_data(input$select_file)
  })
  output$file_valide <- renderText({
    
    if (is.null(data_select())){
      "Aucun fichier selectione"
    }
    else{
      paste("Fichier valide :", nom_data())
    }
  })
  output$file_csv <- renderDT({
    req(input$valide_file)
    data_select()
  })
  
  # Transformation de valeurs en facteur
  
  output$trans_factor <- renderUI({
    req(input$valide_file)
    
    dt <- data_select()
    choices <- names(dt)
    selectInput("choices_trans_factor", "Sélectionnez la variable à transformer en facteur", choices = names(dt))
  })
  observeEvent(input$apply_trans_factor,{
    req(input$choices_trans_factor)
    
    dt <- data_select()
    
    var_factor <- input$choices_trans_factor
    
    dt[[var_factor]] <- as.factor(dt[[var_factor]])
    
    data_select(dt)
  })
  
  # Parti affection valeur pour le facteurs 
  
  valeurs_facteurs <- reactiveValues(data = list())
  
  output$var_categ <- renderUI({
    req(input$choices_trans_factor)
    
    dt <- data_select()
    
    choices <- names(dt)[sapply(dt, is.factor)]
    
    selectInput("select_var_categ", "2. Choisissez une variable catégorielle :", choices = choices)
  })
  output$mod_selector <- renderUI({
    req(input$select_var_categ)
    dt <- data_select()
    choices <- levels(dt[[input$select_var_categ]])
    
    selectInput("selected_mods", "Sélectionnez un sous-ensemble de modalités :",
                choices = choices, multiple = TRUE)
  })
  observeEvent(input$apply_group, {
    req(input$selected_mods)
    for (mod in input$selected_mods) {
      valeurs_facteurs$data[[mod]] <- input$group_value
    }
  })
  output$valeurs_associees <- renderTable({
    req(input$select_var_categ)
    dt <- data_select()
    all_modalities <- levels(dt[[input$select_var_categ]])
    assigned_values <- sapply(all_modalities, function(mod) {
      valeurs_facteurs$data[[mod]] %||% NA
    })
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
  })


  output$filtered_with_values <- renderDT({
    req(input$select_var_categ)
    var <- input$select_var_categ
    
    df <- data_select()
    df$modalite <- df[[var]]
    
    df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
      valeurs_facteurs$data[[mod]] %||% NA
    })
    
    df$Taux_conversion <- ifelse(!is.na(df$valeur_associee) & df$valeur_associee != 0,
                                 1 / df$valeur_associee,
                                 NA)
    
    # Ajout colonne Proidskg si variable quantité sélectionnée
    if (!is.null(input$quant_var) && input$quant_var %in% names(df)) {
      df$Proids_en_kg <- df$Taux_conversion * df[[input$quant_var]]
    } else {
      df$Proids_en_kg <- NA
    }
    
    df$modalite <- NULL
    
    # Ajout colonne poids en kg si variable quantité sélectionnée
    if (!is.null(input$quant_var) && input$quant_var %in% names(df)) {
      df$Proids_en_kg <- df$Taux_conversion * df[[input$quant_var]]
    } else {
      df$Proids_en_kg <- NA
    }
    
    # Ajout colonne prixKilo si variable prix sélectionnée
    if (!is.null(input$price_var) && input$price_var %in% names(df)) {
      df$prixKilo <- ifelse(!is.na(df$Proids_en_kg) & df$Proids_en_kg != 0,
                            df[[input$price_var]] / df$Proids_en_kg,
                            NA)
    } else {
      df$prixKilo <- NA
    }
    
    
    df[, c(var, "valeur_associee", "Taux_conversion", "Proids_en_kg", "prixKilo",
           setdiff(colnames(df), c(var, "valeur_associee", "Taux_conversion", "Proids_en_kg", "prixKilo")))]
    
    
  }, rownames = TRUE, options = list(scrollX = TRUE))
  

  # Étape 4 - Sélection de la variable de quantité
  output$quant_var_selector <- renderUI({
    req(data_select())
    choices <- names(data_select())
    selectInput("quant_var", "4. Sélection de la variable quantité :", choices = choices)
  })
  
  # Étape 5 - Sélection de la variable de prix
  output$price_var_selector <- renderUI({
    req(data_select())
    choices <- names(data_select())
    selectInput("price_var", "5. Sélection de la variable prix :", choices = choices)
  })
  
  

  
  
}

shinyApp(ui, server)
