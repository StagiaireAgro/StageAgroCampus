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
      
      
      numericInput("group_value", "3. Valeur à associer :", value = 0),
      actionButton("apply_group", "Associer aux modalités sélectionnées")
      
      
    ),
    mainPanel(
      h4("Jeu de données"),
      dataTableOutput("file_csv"),
      h4("Affectations actuelles :"),
      tableOutput("valeurs_associees"),
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
      valeurs_facteurs$data[[mod]] %||% NaN
      })
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
    })
  
  
  
 

  
  
}

shinyApp(ui, server)
