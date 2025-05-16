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
                 uiOutput("quant_var_selector"),
                 uiOutput("price_var_selector"),
                 # Transformation de valeurs en facteur
                 uiOutput("trans_factor"),
                 
                 actionButton("apply_trans_factor", "Transformer"),
                 
                 # Partie affection valeur pour le facteurs 
                 uiOutput("var_categ"),
                 
                 uiOutput("mod_selector"),
                 
                 
                 numericInput("group_value", " Combien de cette unité pour 1kg :", value = 0),
                 actionButton("apply_group", "Associer aux modalités sélectionnées"),
                 
                 
                 # Partie 2 affection valeur pour le facteurs 
                 uiOutput("trans_factor2"),
                 actionButton("apply_trans_factor2", "Transformer"),
                 
                 uiOutput("var_categ2"),
                 uiOutput("mod_selector2"),
                 
                 numericInput("group_value2", " Combien de cette unité pour 1kg :", value = 0),
                 numericInput("group_qt2", " Quantité de cette unité :", value = 0),
                 actionButton("apply_group2", "Associer aux modalités sélectionnées")
                 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Choix csv", h4("Jeu de données"),
                 dataTableOutput("file_csv")),
        
        tabPanel("jeu de données filtré",h4("Affectations actuelles :"),
                 tableOutput("valeurs_associees"),
                 dataTableOutput("filtered_with_values")),
        
        tabPanel("Non calculable directement",
                 tableOutput("valeurs_associees2"),
                 dataTableOutput("filtered_with_values2"))
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
  data_c_direct <- reactiveVal()
  
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
      df$Poids_en_kg <- df$Taux_conversion * df[[input$quant_var]]
    } else {
      df$Poids_en_kg <- NA
    }
    
    df$modalite <- NULL
    
    # Ajout colonne poids en kg si variable quantité sélectionnée
    if (!is.null(input$quant_var) && input$quant_var %in% names(df)) {
      df$Poids_en_kg <- df$Taux_conversion * df[[input$quant_var]]
    } else {
      df$Poids_en_kg <- NA
    }
    
    # Ajout colonne prixKilo si variable prix sélectionnée
    if (!is.null(input$price_var) && input$price_var %in% names(df)) {
      df$prixKilo <- ifelse(!is.na(df$Poids_en_kg) & df$Poids_en_kg != 0,
                            df[[input$price_var]] / df$Poids_en_kg,
                            NA)
    } else {
      df$prixKilo <- NA
    }
    
    data_c_direct(df[!is.na(df$Poids_en_kg), c(var, "valeur_associee", "Taux_conversion", "Poids_en_kg", "prixKilo",
                                                setdiff(colnames(df), c(var, "valeur_associee", "Taux_conversion", "Poids_en_kg", "prixKilo")))])
    
    data_nc_direct(df[is.na(df$Poids_en_kg),])
    
    return(df[!is.na(df$Poids_en_kg), c(var, "valeur_associee", "Taux_conversion", "Poids_en_kg", "prixKilo",
                                         setdiff(colnames(df), c(var, "valeur_associee", "Taux_conversion", "Poids_en_kg", "prixKilo")))])
    
  }, rownames = TRUE, options = list(scrollX = TRUE))
  
  output$quant_var_selector <- renderUI({
    req(data_select())
    choices <- names(data_select())
    selectInput("quant_var", "Sélection de la variable quantité :", choices = choices)
  })
  output$price_var_selector <- renderUI({
    req(data_select())
    choices <- names(data_select())
    selectInput("price_var", "Sélection de la variable prix :", choices = choices)
  })
  
  # partie 2
  valeurs_facteurs2 <- reactiveValues()
  data_nc_direct <- reactiveVal()
  
  output$trans_factor2 <- renderUI({
    req(input$apply_group)
    
    dt <- data_nc_direct()
    choices <- names(dt)
    selectInput("choices_trans_factor2", "Sélectionnez la variable à transformer en facteur", choices = names(dt))
  })
  observeEvent(input$apply_trans_factor2,{
    req(input$choices_trans_factor2)
    
    dt <- data_nc_direct()
    
    var_factor <- input$choices_trans_factor2
    
    dt[[var_factor]] <- as.factor(dt[[var_factor]])
    
    data_nc_direct(dt)
  })
  
  output$var_categ2 <- renderUI({
    req(input$apply_group)
    
    dt <- data_nc_direct()
    
    choices <- names(dt)[sapply(dt, is.factor)]
    
    selectInput("select_var_categ2", "3. Choisissez une variable catégorielle :", choices = choices)
  })
  output$mod_selector2 <- renderUI({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    choices <- levels(dt[[input$select_var_categ2]])
    
    selectInput("selected_mods2", "Sélectionnez un sous-ensemble de modalités :",
                choices = choices, multiple = TRUE)
  })
  observeEvent(input$apply_group2, {
    req(input$selected_mods2)
    for (mod in input$selected_mods2) {
      valeurs_facteurs2$data[[mod]] <- input$group_qt2/input$group_value2
    }
  })
  output$valeurs_associees2 <- renderTable({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    all_modalities <- levels(dt[[input$select_var_categ2]])
    assigned_values <- sapply(all_modalities, function(mod) {
      valeurs_facteurs2$data[[mod]] %||% NA
    })
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
  })
  
  # Création du tableau interactif affiché dans l'onglet "Non calculable directement"
  output$filtered_with_values2 <- renderDT({
    
    # Vérifie que la variable catégorielle a bien été sélectionnée (nécessaire pour continuer)
    req(input$select_var_categ2)
    
    # On stocke le nom de la variable catégorielle sélectionnée dans 'var'
    var <- input$select_var_categ2
    
    # On récupère le dataframe des données non calculables directement
    df <- data_nc_direct()
    
    # On crée une nouvelle colonne 'modalite' avec les valeurs de la variable catégorielle sélectionnée
    df$modalite <- df[[var]]
    
    # Pour chaque modalité, on lui associe la valeur enregistrée dans valeurs_facteurs2$data
    # Si aucune valeur n’est associée, on met NA
    df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
      valeurs_facteurs2$data[[mod]] %||% NA
    })
    
    # Si la valeur associée n’est pas NA et différente de 0, on garde sa valeur
    # Sinon, on met NA. Cela permet de calculer le taux de conversion
    df$Taux_conversion <- ifelse(!is.na(df$valeur_associee) & df$valeur_associee != 0,
                                 df$valeur_associee,
                                 NA)
    
    # Si la variable quantité est bien sélectionnée et existe dans le dataframe...
    if (!is.null(input$quant_var) && input$quant_var %in% names(df)) {
      # ...alors on calcule le poids en kg : quantité * taux de conversion
      df$Proids_en_kg <- df$Taux_conversion * df[[input$quant_var]]
    } else {
      # Sinon, on met NA
      df$Proids_en_kg <- NA
    }
    
    # Si la variable prix est bien sélectionnée et existe dans le dataframe...
    if (!is.null(input$price_var) && input$price_var %in% names(df)) {
      # ...alors on calcule le prix au kilo : prix / poids en kg (si non nul)
      df$prixKilo <- ifelse(!is.na(df$Proids_en_kg) & df$Proids_en_kg != 0,
                            df[[input$price_var]] / df$Proids_en_kg,
                            NA)
    } else {
      # Sinon, on met NA
      df$prixKilo <- NA
    }
    
    # On supprime la colonne temporaire 'modalite' (plus nécessaire pour l'affichage final)
    df$modalite <- NULL
    
    # On retourne un dataframe avec :
    # - les colonnes principales : var, valeur associée, taux, poids kg, prix/kg
    # - suivies de toutes les autres colonnes du dataframe (en conservant leur ordre d’origine)
    return(df[, c(var, "valeur_associee", "Taux_conversion", "Proids_en_kg", "prixKilo",
                  setdiff(colnames(df), c(var, "valeur_associee", "Taux_conversion", "Proids_en_kg", "prixKilo")))])
    
    # Options du datatable : affiche les noms de ligne, et permet le scroll horizontal si nécessaire
  }, rownames = TRUE, options = list(scrollX = TRUE))
  
  
  
  
}

shinyApp(ui, server)
