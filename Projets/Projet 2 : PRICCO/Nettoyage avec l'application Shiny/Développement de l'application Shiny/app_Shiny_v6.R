
library(shiny)
library(DT)
library(dplyr)

options(shiny.reactlog = TRUE) # CTRL+F3

files_names <- list.files(path = "data")

path <- "data/cagette"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(h2("Standardisation du conditionnemment"),
                 
                 # Slection du fichier
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("Jeu de données"),
                 
                 
                 
                 uiOutput("file_list"),
                 
                 textInput("sep","Quel est le délimitateur ?", value = ","),
                 textInput("dec","Quel est le séparateur de décimale ?", value = "."),
                 
                 actionButton("valide_file", "Validez le fichier selectionné"),
                 textOutput("file_valide"),
                 
                 # Slection de la varaible quantite du conditionnement
                 
                 uiOutput("selct_var_qt"),
                 
                 # Selection du prix et du nombre d'achat et date
                 
                 uiOutput("price"),
                 uiOutput("order_quantity"),
                 uiOutput("date"),
                 uiOutput("organic"),
                 uiOutput("name_product"),
                 
                 actionButton("apply_var", "Appliquez la sélection"),
                 
                 
                 # Transformation de valeurs en facteur
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("Conversion à partir des unités de mesure"),
                 
                 uiOutput("trans_factor"),
                 actionButton("apply_trans_factor", "Appliquez la  sélection"),
                 
                 # Première affection valeur pour le facteurs 
                 uiOutput("var_categ"),
                 
                 uiOutput("mod_selector"),
                 
                 numericInput("group_value", "Définissez le taux de conversion ad hoc :", value = NA),
                 actionButton("apply_group", "Appliquez la conversion"),
                 
                 # Deuxème transformation en facteur
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("Conversion à  partir des noms du produit"),
                 
                 uiOutput("trans_factor2"),
                 actionButton("apply_trans_factor2", "Appliquez la  sélection"),
                 
                 # Deuxième affection valeur pour le facteurs 
                 
                 uiOutput("var_categ2"),
                 
                 uiOutput("mod_selector2"),
                 
                 numericInput("group_qt2", "Poids/volume associé au produit conditionné :", value = NA),
                 numericInput("group_value2", "Définissez le taux de conversion ad hoc :", value = NA),
                 actionButton("apply_group2", "Appliquez la conversion"),
                 
                 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Jeu de données",
                 dataTableOutput("file_csv")),
        
        tabPanel("Conversion à partir des unités",h4("Affectations actuelles :"),
                 tableOutput("valeurs_associees"),
                 dataTableOutput("filtered_with_values")),
        
        tabPanel("Conversion à partir des noms",h4("Affectations actuelles :"),
                 tableOutput("valeurs_associees2"),
                 dataTableOutput("filtered_with_values2")),
        
        tabPanel("Jeux de données standardisé ou non",
                 uiOutput("cols"),
                 h4("Jeu de données standardisé"),
                 downloadButton("download_calcule","Télécharger"),
                 dataTableOutput("full_filtered"),
                 
                 h4("Jeu de données non standardisé"),
                 downloadButton("download_non_calcule","Télécharger"),
                 dataTableOutput("data_non_calc")),
        tabPanel("Prix au kilo",
                 downloadButton("download_mean_per_months", "Télécharger"),
                 dataTableOutput("prix_au_kilo"),
                 
                 h3("Seuil de sélection"),
                 sliderInput("mult_EIQ", "Nombre d'intervalle(s) inter-quartile :", min = 0, max = 10, value = 1.5, step = 0.25),
                 dataTableOutput("prix_au_kilo_ab"),
                 dataTableOutput("dtout_vab"))
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  data_select <- reactiveVal(NULL)
  nom_data <- reactiveVal(NULL)
  
  # Slection du fichier
  
  output$file_list <- renderUI({
    choice <- list.files(path = path) # On peut changer le chemin
    selectInput("select_file", "Sélectionnez un fichier", choices = choice)
  })
  
  observeEvent(input$valide_file,{
    req(input$select_file)
    
    file_path <- input$select_file
    
    dt <- read.csv(paste(path, "/", input$select_file, sep = ""), sep = input$sep, dec = input$dec) # on peut changer le chemin
    
    data_select(dt)
    nom <- substr(input$select_file, 1, nchar(input$select_file)-4) # On enlève le .csv
    nom_data(nom)
  })
  
  output$file_valide <- renderText({
    
    if (is.null(data_select())){
      "Aucun fichier selectionné"
    }
    else{
      paste("Fichier validé :", nom_data())
    }
  })
  
  output$file_csv <- renderDT({
    req(input$valide_file)
    data_select()
  })
  
  # Selection du la variable quantite de 'unite
  
  output$selct_var_qt <- renderUI({
    req(input$valide_file)
    
    dt <- data_select()
    choice <- names(dt)
    selectInput("var_qt", "Sélectionnez la variable qui correspond au poids d'une unité de produit :", choices = choice, selected = NULL)
    
  })
  
  output$name_product <- renderUI({
    req(input$valide_file)
    
    choices <- names(data_select())
    
    selectInput("selected_name_product", "Sélectionnez la variable qui correspond au type de produit :", choices = choices, selected = NULL)
  })
  
  # Selection de la vairable prix
  
  output$price <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_price", "Sélectionnez la variable qui correspond au prix d'une unité de produit :", choices = choices)
  })
  
  # Sélection du nombre d'achat du produit
  
  output$order_quantity <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_order_quantity", "Sélectionnez la variable qui correspond au nombre d'une unité de produit acheté :", choices = choices)
  })
  
  # Sélection de la date
  
  output$date <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_date", "Sélectionnez la variable qui correspond à la date d'achat :", choices = choices)
  })
  
  # Selection bio
  
  output$organic <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_organic", "Sélectionnez la variable qui désigne si le produit est bio :", choices = choices)
  })
  
  var_qt <- reactiveVal(NULL)
  
  order_qt <- reactiveVal(NULL)
  
  order_price <- reactiveVal(NULL)
  
  order_date <- reactiveVal(NULL)
  
  organic <- reactiveVal(NULL)
  
  name_product <- reactiveVal(NULL)
  
  observeEvent(input$apply_var, {
    
    var_qt(input$var_qt)
    
    order_qt(input$selected_order_quantity)
    
    order_price(input$selected_price)
    
    order_date(input$selected_date)
    
    organic(input$selected_organic)
    
    name_product(input$selected_name_product)
  })
  
  # Transformation de valeurs en facteur
  
  output$trans_factor <- renderUI({
    req(input$valide_file)
    
    dt <- data_select()
    choices <- names(dt)
    selectInput("choices_trans_factor", "Sélectionnez la variable qui correspond  aux unités de mesure :", choices = names(dt))
  })
  observeEvent(input$apply_trans_factor,{
    req(input$choices_trans_factor)
    
    dt <- data_select()
    
    var_factor <- input$choices_trans_factor
    
    dt[[var_factor]] <- as.factor(dt[[var_factor]])
    
    data_select(dt)
  })
  
  # Première affection valeur pour le facteurs 
  
  valeurs_facteurs <- reactiveValues(data = list())
  
  output$var_categ <- renderUI({
    req(input$choices_trans_factor)
    
    dt <- data_select()
    
    choices <- names(dt)[sapply(dt, is.factor)]
    
    selectInput("select_var_categ", "La variable qui correspond aux unités de mesure est :", choices = choices, selected = NULL)
  })
  output$mod_selector <- renderUI({
    req(input$select_var_categ)
    dt <- data_select()
    choices <- levels(dt[[input$select_var_categ]])
    
    selectInput("selected_mods", "Sélectionnez les unités de mesure à agréger :",
                choices = choices, multiple = TRUE)
  })
  observeEvent(input$apply_group, {
    req(input$selected_mods)
    
    for (mod in input$selected_mods) {
      valeurs_facteurs$data[[mod]] <- input$group_value
    }
    
  })
  # Première association de valeurs en dataframe
  
  output$valeurs_associees <- renderTable({
    req(input$select_var_categ)
    dt <- data_select()
    all_modalities <- levels(dt[[input$select_var_categ]])
    assigned_values <- sapply(all_modalities, function(mod) {
      valeurs_facteurs$data[[mod]] %||% NA
    })
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
  })
  
  # Premier calcul du poid en kg conditionné
  
  data_c_direct <- reactiveVal()
  data_nc_direct <- reactiveVal()
  
  output$filtered_with_values <- renderDT({
    req(input$select_var_categ)
    var <- input$select_var_categ
    
    df <- data_select()
    df$modalite <- df[[var]]
    
    df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
      valeurs_facteurs$data[[mod]] %||% NA
    })
    
    df$kg_condit <- as.numeric(df[[var_qt()]])/df$valeur_associee
    
    data_c_direct(df[!is.na(df$valeur_associee),]) # On affecte les données qui sont calculable directement
    data_nc_direct(df[is.na(df$valeur_associee),])
    
    return(df[!is.na(df$valeur_associee),])
    
  })
  
  # Deuxième transformation en facteur
  
  output$trans_factor2 <- renderUI({
    req(input$valide_file)
    
    dt <- data_nc_direct()
    choices <- names(dt)
    selectInput("choices_trans_factor2", "Sélectionnez la variable qui correspond aux noms du produit :", choices = names(dt))
  })
  observeEvent(input$apply_trans_factor2,{
    req(input$choices_trans_factor2)
    
    dt <- data_nc_direct()
    
    var_factor <- input$choices_trans_factor2
    
    dt[[var_factor]] <- as.factor(dt[[var_factor]])
    
    data_nc_direct(dt)
  })
  
  # Deuxième affection valeurs pour les facteurs
  
  valeurs_facteurs2 <- reactiveValues(data = list())
  
  output$var_categ2 <- renderUI({
    
    dt <- data_nc_direct()
    
    choices <- names(dt)[sapply(dt, is.factor)]
    
    selectInput("select_var_categ2", "La variable qui correspond aux noms du produit est :", choices = choices) # N'est pas possible d'acces tant que l'on n'est pas sur le deuxième page car la creation du deuxième dataframe se fait à l'affichage de celui ci
  })
  
  output$mod_selector2 <- renderUI({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    choices <- levels(dt[[input$select_var_categ2]])
    
    selectInput("selected_mods2", "Sélectionnez les noms à agréger :",
                choices = choices, multiple = TRUE)
  })
  
  refresh_table2 <- reactiveVal(0) # varibale de refresh pour le 3eme tableau
  
  observeEvent(input$apply_group2, {
    req(input$selected_mods2)
    current <- valeurs_facteurs2$data
    for (mod in input$selected_mods2) {
      current[[mod]] <- input$group_qt2 / input$group_value2
    }
    valeurs_facteurs2$data <- current
    
    refresh_table2(refresh_table2() + 1) # refresh a chaque apply_group2
    
  })
  
  # Deuxième association de valeurs en dataframe
  
  output$valeurs_associees2 <- renderTable({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    all_modalities <- levels(dt[[input$select_var_categ2]])
    assigned_values <- sapply(all_modalities, function(mod) {
      valeurs_facteurs2$data[[mod]] %||% NA
    })
    data.frame(Modalité = all_modalities, Valeur = assigned_values)
  })
  
  # Deuxième calcul du poid en kg conditionné
  
  data_nc_direct_final <- reactiveVal(NULL)
  
  data_non_calc <- reactiveVal()
  
  output$filtered_with_values2 <- renderDT({
    refresh_table2() # refresh de la table pour chaque apply
    input$apply_group2
    
    req(input$select_var_categ2)
    var <- input$select_var_categ2
    
    df <- data_nc_direct()
    df$modalite <- df[[var]]
    
    df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
      valeurs_facteurs2$data[[mod]] %||% NA
    })
    
    df$kg_condit <- as.numeric(df[[var_qt()]])*df$valeur_associee
    
    data_nc_direct_final(df[!is.na(df$valeur_associee),]) # On affecte les données qui ne sont pas calculable directement
    data_non_calc(df[is.na(df$valeur_associee),]) # Données non calculable
    
    return(df[!is.na(df$valeur_associee),])
    
  })
  
  # Rassemblement des deux dataframe calculé et creation de bouton d'export de celui ci et de celui qui n'est pas calculé.
  
  dt_final <- reactiveVal(NULL)
  
  output$full_filtered <- renderDT({
    dt1 <- data_c_direct()
    dt2 <- data_nc_direct_final()
    
    # Vérifier si dt1 et dt2 sont NULL ou vides avant de faire rbind
    if (is.null(dt1) && is.null(dt2)) {
      final_df <- data.frame() # Retourner un dataframe vide si les deux sont NULL
    } else if (is.null(dt1)) {
      final_df <- dt2
    } else if (is.null(dt2)) {
      final_df <- dt1
    } else {
      final_df <- rbind(dt1, dt2)
    }
    
    dt_final(final_df) 
    final_df[,input$selected_cols]
  })
  
  output$data_non_calc <- renderDT({
    data_non_calc()[,input$selected_cols]
  })
  
  # Colonnes que l'on garde
  
  output$cols <- renderUI({
    req(dt_final())
    
    choices <-  names(dt_final())
    
    selectInput("selected_cols", "Sélectionnez les colonnes que vous voulez garder :", choices = choices, selected = choices, multiple = T)
  })
  
  # Boutons de téléchargement
  
  output$download_calcule <- downloadHandler(
    filename = function() {
      paste0(nom_data(),"_OK",".csv")
    },
    content = function(file) {
      write.csv(dt_final()[,input$selected_cols], file, row.names = FALSE)
    })
  
  output$download_non_calcule <- downloadHandler(
    filename = function() {
      paste0(nom_data(),"_PAS_OK",".csv")
    },
    content = function(file) {
      write.csv(data_non_calc()[,input$selected_cols], file, row.names = FALSE)
    })
  
  # calcul du prix au kilo (moyenne ponderee par mois)
  
  dt_vab <- reactiveVal(NULL)
  dt_months <- reactiveVal(NULL)
  dt_vab_m <- reactiveVal(NULL)
  
  output$prix_au_kilo <- renderDT({
    req(dt_final())
    req(name_product())
    
    dt <- dt_final()
    
    dt$pp_kilo <- dt[[order_price()]]/dt$kg_condit
    
    # Calcul des mois
    
    dt$month <- format(as.Date(dt[[order_date()]]), "%m-%Y")
    
    # ENLEVER VALEURS ABERANTES
    
    e_i_q <- quantile(dt$pp_kilo, 0.75) - quantile(dt$pp_kilo, 0.25)
    
    dt_clean <- dt[(dt$pp_kilo <= quantile(dt$pp_kilo, 0.75) + input$mult_EIQ*e_i_q & dt$pp_kilo >= quantile(dt$pp_kilo, 0.25) - input$mult_EIQ*e_i_q),]
    
    dt_nclean <- dt[(dt$pp_kilo > quantile(dt$pp_kilo, 0.75) + input$mult_EIQ*e_i_q | dt$pp_kilo < quantile(dt$pp_kilo, 0.25) - input$mult_EIQ*e_i_q),]
    
    dt_vab(dt_nclean)
    
    # CALCUL MOYENNE PONDEREE PAR MOIS bio/non bio/total pour données non aberrantes
    
    dt_m <- dt_clean %>%
      group_by(.data[[name_product()]], month) %>%
      summarise(
        moyenne_ponderee_totale = sum(pp_kilo * .data[[order_qt()]]*kg_condit, na.rm = TRUE) /
          sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),
        
        quantite_vendu_kg = sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),
        
        moyenne_ponderee_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == TRUE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        quantite_vendu_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        moyenne_ponderee_non_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == FALSE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        quantite_vendu_non_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # CALCUL MOYENNE PONDEREE PAR MOIS bio/non bio/total pour données aberrantes
    
    dt_m_ab <- dt_nclean %>%
      group_by(.data[[name_product()]], month) %>%
      summarise(
        moyenne_ponderee_totale = sum(pp_kilo * .data[[order_qt()]]*kg_condit, na.rm = TRUE) /
          sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),
        
        quantite_vendu_kg = sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),
        
        moyenne_ponderee_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == TRUE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        quantite_vendu_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        moyenne_ponderee_non_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == FALSE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        quantite_vendu_non_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        .groups = "drop"
      )
    
    dt_vab_m(dt_m_ab)
    
    dt_months(data.frame(dt_m))
    
    return(dt_m)
  })
  
  # Téléchargement MOYENNE PONDEREE PAR MOIS bio/non bio/total
  
  output$download_mean_per_months <- downloadHandler(
    filename = function() {
      paste0(nom_data(),"_mean_ppk_per_months",".csv")
    },
    content = function(file) {
      write.csv(dt_months(), file, row.names = FALSE)
    })
  
  # Affichage MOYENNE PONDEREE PAR MOIS bio/non bio/total
  
  output$prix_au_kilo_ab <- renderDT({
    req(dt_vab_m)
    
    dt_vab_m()
  })
  
  output$dtout_vab <- renderDT({
    req(dt_vab())
    
    dt_vab()
  })
  
}

shinyApp(ui, server)
