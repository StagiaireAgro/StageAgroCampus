library(shiny)

# Exemple de dataset : abricot_2021

data_abricot_2021 <- read.csv("cagette_Abricot_2021.csv")
data_abricot_2021$productConditioningUnit <- as.factor(data_abricot_2021$productConditioningUnit)

ui <- fluidPage(
  titlePanel("Taux de conversion"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_categ", "1. Choisissez une variable catégorielle :",
                  choices = names(data_abricot_2021)[sapply(data_abricot_2021, is.factor)]),
      
      uiOutput("mod_selector"),
      
      numericInput("group_value", "2. Valeur à associer :", value = 0),
      actionButton("apply_group", "Associer aux modalités sélectionnées")
    ),
    mainPanel(
      h4("Affectations actuelles :"),
      tableOutput("valeurs_associees"),
      
      h4("Jeu de données avec valeurs associées :"),
      tableOutput("filtered_with_values")
    )
  )
)

server <- function(input, output, session) {
  
  valeurs <- reactiveValues(data = list())
  
  output$mod_selector <- renderUI({
    req(input$var_categ)
    choices <- levels(data_abricot_2021[[input$var_categ]])
    
    selectInput("selected_mods", "Sélectionnez un sous-ensemble de modalités :",
                choices = choices, multiple = TRUE)
  })
  
  observeEvent(input$apply_group, {
    req(input$selected_mods)
    for (mod in input$selected_mods) {
      valeurs$data[[mod]] <- input$group_value
    }
  })
  
  output$valeurs_associees <- renderTable({
    req(input$var_categ)
    all_modalities <- levels(data_abricot_2021[[input$var_categ]])
    assigned_values <- sapply(all_modalities, function(mod) {
      valeurs$data[[mod]] %||% NA
    })
    
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
  })
  
  output$filtered_with_values <- renderTable({
    req(input$var_categ)
    var <- input$var_categ
    all_modalities <- levels(data_abricot_2021[[var]])
    
    df <- data_abricot_2021
    df$modalite <- df[[var]]
    
    df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
      valeurs$data[[mod]] %||% NA
    })
    
    df[, c(var, "valeur_associee", setdiff(colnames(df), c(var, "valeur_associee")))]
  }, rownames = TRUE)
}

shinyApp(ui, server)
