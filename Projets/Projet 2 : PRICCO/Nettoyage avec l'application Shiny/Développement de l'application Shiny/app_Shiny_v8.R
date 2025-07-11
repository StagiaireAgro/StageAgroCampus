
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(shinyWidgets)

options(shiny.reactlog = TRUE) # CTRL+F3

files_names <- list.files(path = "data")

path <- "data/cagette"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(h2("PRICCO - Standardisation du conditionnemment"),
                 
                 # Selection du fichier
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("1 - Import du jeu de données"),
                 
                 fileInput("select_file", "Sélectionnez un fichier :",
                           multiple = FALSE,
                           buttonLabel = "Parcourir...",
                           placeholder = "Aucun fichier sélectionné",
                           accept = c(".csv", ".txt")),
                 div(
                   style = "display: flex; gap: 20px;",
                   
                   div(
                     style = "flex: 1;",
                     textInput("sep", "Délimiteur de colonnes :", value = ",", width = "100%")
                   ),
                   
                   div(
                     style = "flex: 1;",
                     textInput("dec", "Séparateur de décimale :", value = ".", width = "100%")
                   )
                 ),
                 
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("valide_file", "Valider le fichier sélectionné", 
                                style = "background-color: #28a745; color: white; border: none; padding: 10px 20px; font-weight: bold;")
                 ),
                 div(style = "text-align: center; margin-top: 10px;", uiOutput("file_valide")),
                 br(),
                
                 # Selection du prix et du nombre d'achat et date
                 
                 uiOutput("price"),
                 uiOutput("order_quantity"),
                 uiOutput("date"),
                 uiOutput("organic"),
                 uiOutput("selct_var_qt"),
                 uiOutput("plateforme"),
                 uiOutput("name_product"),
                 
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("apply_var", "Valider les correspondances")
                 ),
                 
                 # Transformation de valeurs en facteur
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("2 - Conversion des unités de mesure"),
                 
                 uiOutput("trans_factor"),
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("apply_trans_factor", "Valider la correspondance")
                 ),
                 br(),
                 
                 # Première affection valeur pour le facteurs 
                 uiOutput("var_categ"),
                 
                 uiOutput("mod_selector"),
                 
                 selectInput(
                   "group_value",
                   "Taux de conversion vers Kg ou L",
                   choices = c("1000 (g, mL)" = 1000,"100 (dg, cL)" = 100,"10 (cg, dL)" = 10,"1 (Kg, L)" = 1),selected = "1",width = "100%"),
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("apply_group","Convertir",
                     style = "background-color: #28a745;color: white;border: none;padding: 10px 20px;font-weight: bold;"
                   )
                 ),
                 
                 # Deuxème transformation en facteur
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("3 - Conversion à  partir des noms du produit"),
                 
                 uiOutput("trans_factor2"),
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("apply_trans_factor2", "Valider la correspondance")
                 ),
                 br(),
                 
                 # Deuxième affection valeur pour le facteurs 
                 
                 uiOutput("var_categ2"),
                 
                 uiOutput("mod_selector2"),
                 
                 uiOutput("group_qt2_ui"),
                 
                 selectInput(
                   "group_value2",
                   "Taux de conversion vers Kg ou L",
                   choices = c("1000 (g, mL)" = 1000,"100 (dg, cL)" = 100,"10 (cg, dL)" = 10,"1 (Kg, L)" = 1),selected = "1",width = "100%"),
                 
                 div(
                   style = "text-align: center; margin-top: 10px;",
                   actionButton("apply_group2","Convertir",
                                style = "background-color: #28a745;color: white;border: none;padding: 10px 20px;font-weight: bold;"
                   )
                 ),
                 
                 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1 - Import",
                 h3("Données importées"),
                 dataTableOutput("file_csv")),
        
        tabPanel("2 - Conversion à partir des unités",
                 h3("Affectations actuelles"),
                 tableOutput("valeurs_associees"),
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h3("Lignes converties"),
                 dataTableOutput("filtered_with_values")),
        
        tabPanel("3 - Conversion à partir des noms",
                 h3("Affectations actuelles"),
                 tableOutput("valeurs_associees2"),
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h3("Lignes converties"),
                 dataTableOutput("filtered_with_values2")),
        
        tabPanel("Jeu de données clean",
                 div(
                   style = "display: flex; align-items: center; justify-content: space-between;",
                   h3("Jeu de données standardisé"),
                   downloadButton("download_calcule", "Télécharger", class = "btn-primary")
                 ),
                 uiOutput("cols"),
                 dataTableOutput("full_filtered"),
                 tags$hr(style = "border-top: 2px solid #999;"),
                 div(
                   style = "display: flex; align-items: center; justify-content: space-between;",
                   h3("Jeu de données non-standardisé"),
                   downloadButton("download_non_calcule", "Télécharger", class = "btn-primary")
                 ),
                 dataTableOutput("data_non_calc")),
        tabPanel("Prix au kilo",
                 div(
                   style = "display: flex; align-items: center; justify-content: space-between;",
                   h3("Moyenne avec seuil (sans valeurs aberrantes)"),
                   downloadButton("download_mean_per_months", "Télécharger", class = "btn-primary")
                 ),
                 h4("Seuil de sélection"),
                 sliderInput("mult_EIQ", "Nombre d'intervalle(s) inter-quartile :", min = 0, max = 10, value = 1.5, step = 0.25),
                 dataTableOutput("prix_au_kilo"),
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("Lignes inclues"),
                 dataTableOutput("dtout_v"),
                 h4("Lignes exclues"),
                 dataTableOutput("dtout_vab"))
      )
      
    )
    
  )
  
)

options(shiny.maxRequestSize = 30 * 1024^2)  # 30 Mo

server <- function(input, output, session) {
  
  data_select <- reactiveVal(NULL)
  nom_data <- reactiveVal(NULL)
  file_validated <- reactiveVal(FALSE)  # vrai si fichier chargé avec succès
  validated_clicked <- reactiveVal(FALSE) # vrai si bouton validé cliqué
  
  # Selection du fichier
  
  observeEvent(input$valide_file,{
    validated_clicked(TRUE)
    req(input$select_file)
    
    file_path <- input$select_file$datapath
    file_name <- input$select_file$name
    
    dt <- read.csv(file_path, sep = input$sep, dec = input$dec)
    
    data_select(dt)
    nom <- substr(file_name, 1, nchar(file_name) - 4) # On enlève le .csv
    nom_data(nom)
    file_validated(TRUE)
  })
  
  output$file_valide <- renderUI({
    if (!validated_clicked()) {
      return(NULL)  # Rien si pas encore cliqué
    }
    
    if (!file_validated()) {
      tags$em("Aucun fichier sélectionné")  # Texte en italique
    } else {
      tags$em("Fichier chargé")  # Texte en italique
    }
  })
  
  output$file_csv <- DT::renderDataTable({
    req(file_validated())  # attend que le fichier soit validé (chargé)
    req(data_select())     # attend que les données soient chargées
    
    DT::datatable(data_select())
  })
  
  # Selection de la variable prix
  
  output$price <- renderUI({
    req(data_select())
    choices <- names(data_select())
    default_choice <- choices[grepl("price|prix", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Prix d'une unité de produit :", `for` = "selected_price", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_price", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  # Sélection du nombre d'achat du produit
  
  output$order_quantity <- renderUI({
    req(data_select())
    choices <- names(data_select())
    default_choice <- choices[grepl("orderquantity", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Nb de commandes :", `for` = "selected_order_quantity", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_order_quantity", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  # Sélection de la date
  
  output$date <- renderUI({
    req(data_select())
    choices <- names(data_select())
    default_choice <- choices[grepl("date", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Date :", `for` = "selected_date", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_date", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  # Selection bio
  
  output$organic <- renderUI({
    req(data_select())
    choices <- names(data_select())
    default_choice <- choices[grepl("organic|bio", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Bio (0/1) :", `for` = "selected_organic", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_organic", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  # Selection du la variable quantite de l'unite
  
  output$selct_var_qt <- renderUI({
    
    req(input$valide_file)
    dt <- data_select()
    choices <- names(dt)
    default_choice <- choices[grepl("conditioningquantity", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Poids d'une unité de produit :", `for` = "var_qt", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("var_qt", label = NULL, choices = choices, selected = selected_default)
        )
    )
    
  })
  
  #Sélection de la plateforme
  
  output$plateforme <- renderUI({
    req(input$valide_file)
    choices <- names(data_select())
    default_choice <- choices[grepl("plateforme", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Nom de la plateforme :", `for` = "selected_plateforme", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_plateforme", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  #Sélection du type de produit
  
  output$name_product <- renderUI({
    req(input$valide_file)
    choices <- names(data_select())
    default_choice <- choices[grepl("nom_produit", choices, ignore.case = TRUE)]
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Type de produit :", `for` = "selected_name_product", style = "margin: 0;"),
        div(style = "flex: 1;",
            selectInput("selected_name_product", label = NULL, choices = choices, selected = selected_default)
        )
    )
  })
  
  var_qt <- reactiveVal(NULL)
  
  order_qt <- reactiveVal(NULL)
  
  order_price <- reactiveVal(NULL)
  
  order_date <- reactiveVal(NULL)
  
  organic <- reactiveVal(NULL)
  
  plateforme <- reactiveVal(NULL)
  
  name_product <- reactiveVal(NULL)
  
  observeEvent(input$apply_var, {
    
    var_qt(input$var_qt)
    
    order_qt(input$selected_order_quantity)
    
    order_price(input$selected_price)
    
    order_date(input$selected_date)
    
    organic(input$selected_organic)
    
    plateforme(input$selected_plateforme)
    
    name_product(input$selected_name_product)
    
  })
  
  # Transformation de valeurs en facteur
  
  output$trans_factor <- renderUI({
    req(input$valide_file)

    dt <- data_select()
    choices <- names(dt)
    # On vérifie si la colonne "productConditioningUnit" existe
    selected_default <- if ("productConditioningUnit" %in% choices) {
      "productConditioningUnit"
    } else {
      choices[1]  # Fallback si la colonne n'existe pas
    }
    
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Unité du conditionnement :", `for` = "choices_trans_factor", style = "margin: 0; white-space: nowrap;"),
        div(style = "flex: 1;",
            selectInput("choices_trans_factor", label = NULL, choices = names(dt), selected = selected_default)
        )
    )
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
    
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("L'unité du conditionnement est :", `for` = "select_var_categ", style = "margin: 0; white-space: nowrap;"),
        div(style = "flex: 1;",
            selectInput("select_var_categ", label = NULL, choices = choices, selected = NULL)
        )
    )
  })
  
  output$mod_selector <- renderUI({
    req(input$select_var_categ)
    dt <- data_select()
    choices <- levels(dt[[input$select_var_categ]])
    
    div(style = "display: flex; align-items: center; gap: 10px;",
        tags$label("Sélectionnez les unités à agréger :", `for` = "selected_mods", style = "margin: 0; white-space: nowrap;"),
        div(style = "flex: 1;",
            pickerInput("selected_mods", label = NULL, choices = choices, multiple = TRUE)
        )
    )
  })
  
  observeEvent(input$apply_group, {
    req(input$selected_mods)
    
    for (mod in input$selected_mods) {
      valeurs_facteurs$data[[mod]] <- as.numeric(input$group_value)
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
    default_choice <- choices[grepl("productname", choices, ignore.case = TRUE)] #je voulais mettre clean_name à la place de productname mais ça ne fonctionne étrangement pas...
    selected_default <- if (length(default_choice) > 0) default_choice[1] else choices[1]
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      div(style = "white-space: nowrap;", "Nom du produit :"),
      div(style = "flex: 1;",
          selectInput("choices_trans_factor2", label = NULL, choices = names(dt), selected = selected_default)
      )
    )
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
    
    dt <- data_nc_direct()  # Charger les données
    choices <- names(dt)[sapply(dt, is.factor)]  # Sélectionner les colonnes de type facteur
    
    tagList(
      div(
        style = "display: flex; align-items: center; gap: 10px;",  # Flexbox pour aligner les éléments horizontalement
        div(style = "white-space: nowrap; font-weight: bold;", "Le nom du produit est :"),  # Texte en gras
        div(style = "flex-grow: 1;",  # La case de sélection prend l'espace restant
            selectInput("select_var_categ2", NULL, choices = choices)  # SelectInput sans le label visible
        )
      )
    )
  })
  
  # Deuxième choix : noms des produits dont on souhaite convertir le poids
  
  output$mod_selector2 <- renderUI({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    choices <- levels(dt[[input$select_var_categ2]])
    
    # Filtrer les noms contenant au moins un chiffre
    choices_filtered <- choices[grepl("\\d", choices)]  # Utilisation d'une expression régulière pour trouver les chiffres
    
    # Créer une fonction pour extraire la chaîne numérique la plus longue
    extract_longest_numeric <- function(name) {
      nums <- str_extract_all(name, "\\d+")  # Extraire tous les nombres
      if (length(nums) > 0) {
        max(sapply(nums, nchar))  # Retourner la longueur de la chaîne numérique la plus longue
      } else {
        0  # Si pas de chiffre, retourner 0
      }
    }
    
    # Créer une fonction pour extraire la plus grande valeur numérique
    extract_max_numeric_value <- function(name) {
      nums <- str_extract_all(name, "\\d+")  # Extraire tous les nombres
      if (length(nums) > 0) {
        max(as.numeric(nums[[1]]))  # Trouver la valeur numérique maximale
      } else {
        0  # Si pas de chiffre, retourner 0
      }
    }
    
    # Trier les choix d'abord par la longueur de la chaîne numérique la plus longue, puis par la valeur numérique maximale
    choices_sorted <- choices_filtered[order(
      sapply(choices_filtered, extract_longest_numeric),  # Trier par longueur de la chaîne numérique
      sapply(choices_filtered, extract_max_numeric_value),  # Puis trier par la valeur numérique maximale
      decreasing = TRUE  # Les deux tris seront faits en ordre décroissant
    )]
    
    tagList(
      pickerInput("selected_mods2", "Sélectionnez les noms à agréger :",
                  choices = choices_sorted, multiple = TRUE),
      div(
        style = "text-align: center; margin-top: 10px;",
        actionButton("reset_mods2", "Réinitialiser la sélection")
      )
    )
  })
  
  observeEvent(input$reset_mods2, {
    updatePickerInput(session, "selected_mods2", selected = character(0))
  })

  # Rendre dynamiquement l'input avec les boutons
  output$group_qt2_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: center; align-items: flex-start; gap: 20px; margin-top: 10px;",
        
        # Bouton "-"
        actionButton("decrease_qt", "-", 
                     style = "
            width: 45px; 
            height: 45px; 
            font-size: 26px; 
            background-color: transparent; 
            color: black; 
            border: none; 
            box-shadow: none;
            line-height: 1;
            padding-top: 0;
          "),
        
        # Champ numérique
        numericInput("group_qt2", label = NULL, value = NA, step = 50, min = 0, width = "100px"),
        
        # Bouton "+"
        actionButton("increase_qt", "+", 
                     style = "
            width: 45px; 
            height: 45px; 
            font-size: 26px; 
            background-color: transparent; 
            color: black; 
            border: none; 
            box-shadow: none;
            line-height: 1;
            padding-top: 0;
          ")
      ),
      
      # Style du champ numericInput (pas de bordure, centré)
      tags$style(HTML("
        #group_qt2 {
          text-align: center;
          border: none !important;
          box-shadow: none !important;
        }
        #group_qt2:focus {
          outline: none !important;
        }
      "))
    )
  })
  
  # Logique de + / -
  observeEvent(input$decrease_qt, {
    val <- ifelse(is.na(input$group_qt2), 0, input$group_qt2)
    updateNumericInput(session, "group_qt2", value = max(val - 50, 0))
  })
  
  observeEvent(input$increase_qt, {
    val <- ifelse(is.na(input$group_qt2), 0, input$group_qt2)
    updateNumericInput(session, "group_qt2", value = val + 50)
  })
  
  
  # Affichage de la table en dessous
  
  refresh_table2 <- reactiveVal(0) # varibale de refresh pour le 3eme tableau
  
  observeEvent(input$apply_group2, {
    req(input$selected_mods2)
    current <- valeurs_facteurs2$data
    for (mod in input$selected_mods2) {
      current[[mod]] <- as.numeric(input$group_qt2) / as.numeric(input$group_value2)
    }
    valeurs_facteurs2$data <- current
    
    refresh_table2(refresh_table2() + 1) # refresh a chaque apply_group2
    
  })
  
  # Deuxième association de valeurs en dataframe
  
  output$valeurs_associees2 <- renderTable({
    req(input$select_var_categ2)
    dt <- data_nc_direct()
    all_modalities <- levels(dt[[input$select_var_categ2]])
    
    # Filtrer les noms contenant au moins un chiffre (comme dans mod_selector2)
    choices_filtered <- all_modalities[grepl("\\d", all_modalities)]
    
    # Fonction pour extraire la longueur de la plus longue chaîne numérique
    extract_longest_numeric <- function(name) {
      nums <- str_extract_all(name, "\\d+")[[1]]
      if (length(nums) > 0) {
        max(nchar(nums))
      } else {
        0
      }
    }
    
    # Fonction pour extraire la valeur numérique maximale
    extract_max_numeric_value <- function(name) {
      nums <- str_extract_all(name, "\\d+")[[1]]
      if (length(nums) > 0) {
        max(as.numeric(nums))
      } else {
        0
      }
    }
    
    # Appliquer le tri comme dans mod_selector2
    choices_sorted <- choices_filtered[order(
      sapply(choices_filtered, extract_longest_numeric),
      sapply(choices_filtered, extract_max_numeric_value),
      decreasing = TRUE
    )]
    
    # Récupérer les valeurs associées à chaque modalité
    assigned_values <- sapply(choices_sorted, function(mod) {
      valeurs_facteurs2$data[[mod]] %||% NA
    })
    
    data.frame(Modalité = choices_sorted, Valeur = assigned_values)
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
    
    tagList(
      tags$br(),  # <- saut de ligne
      selectInput(
        "selected_cols",
        "Sélectionnez les colonnes que vous voulez garder :",
        choices = choices,
        selected = choices,
        multiple = TRUE,
        width = "100%"
      )
    )
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
  
  dt_v <- reactiveVal(NULL)
  dt_vab <- reactiveVal(NULL)
  dt_months <- reactiveVal(NULL)
  dt_vab_m <- reactiveVal(NULL)
  
  output$prix_au_kilo <- renderDT({
    req(dt_final())
    req(plateforme())
    req(name_product())
    
    dt <- dt_final()
    
    dt$pp_kilo <- dt[[order_price()]]/dt$kg_condit
    
    # Calcul des mois

    dt$month <- format(as.Date(dt[[order_date()]], format = "%d-%m-%Y"), "%Y-%m") #manon : j'ai interverti année et mois pour que ça trie mieux dans les tableaux
    
    # ENLEVER VALEURS ABERANTES
    
    e_i_q <- quantile(dt$pp_kilo, 0.75, na.rm = TRUE) - quantile(dt$pp_kilo, 0.25, na.rm = TRUE)
    
    dt_clean <- dt[(dt$pp_kilo <= quantile(dt$pp_kilo, 0.75, na.rm = TRUE) + input$mult_EIQ*e_i_q & dt$pp_kilo >= quantile(dt$pp_kilo, 0.25, na.rm = TRUE) - input$mult_EIQ*e_i_q),]
    
    dt_nclean <- dt[(dt$pp_kilo > quantile(dt$pp_kilo, 0.75, na.rm = TRUE) + input$mult_EIQ*e_i_q | dt$pp_kilo < quantile(dt$pp_kilo, 0.25, na.rm = TRUE) - input$mult_EIQ*e_i_q),]
    
    dt_v(dt_clean)
    
    dt_vab(dt_nclean)
    
    # CALCUL MOYENNE PONDEREE PAR MOIS bio/non bio/total pour données non aberrantes
    
    dt_m <- dt_clean %>%
      group_by(.data[[name_product()]], month) %>%
      summarise(
        moy_tot = round(sum(pp_kilo * .data[[order_qt()]]*kg_condit, na.rm = TRUE) /
          sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),2),
        
        qte_vendue = round(sum(.data[[order_qt()]]*kg_condit, na.rm = TRUE),2),
        
        moy_bio = round(sum(pp_kilo[as.logical(.data[[organic()]]) == TRUE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),2),
        
        qte_vendue_bio = round(sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*kg_condit[as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),2),
        
        moy_conv = round(sum(pp_kilo[as.logical(.data[[organic()]]) == FALSE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),2),
        
        qte_vendu_conv = round(sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*kg_condit[as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),2),
        
        .groups = "drop"
      )
    
    dt_months(data.frame(dt_m))
    
    DT::datatable(
      dt_m,
      options = list(
        pageLength = 12,
        lengthMenu = list(c(12, 24, 36, 48, 60), c("12", "24", "36", "48", "60")),
        searching = FALSE  # enlève la barre de recherche
      )
    )
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
  
  output$dtout_v <- renderDT({
    req(dt_v())
    
    dt_v()
  })
  
  output$dtout_vab <- renderDT({
    req(dt_vab())
    
    dt_vab()
  })
  
}

shinyApp(ui, server)
