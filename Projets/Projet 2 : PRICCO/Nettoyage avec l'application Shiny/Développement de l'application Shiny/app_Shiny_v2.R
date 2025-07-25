library(shiny)
library(DT)
library(dplyr)

path <- "clean_data/brute/cagette"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Calcul du Kg conditionné",
                 
                 # Slection du fichier
                 tags$hr(style = "border-top: 2px solid #999;"),
                 h4("Jeu de données"),
                 
                 
                 
                 uiOutput("file_list"),
                 
                 textInput("sep","Quel est le délimitateur ?", value = ","),
                 textInput("dec","Quel est le séparateur de décimale ?", value = "."),
                 
                 actionButton("valide_file", "Valider le fichier selectione"),
                 textOutput("file_valide"),
                 
                 # Slection de la varaible quantite du conditionnement
                 
                 uiOutput("selct_var_kg"),
                 
                 # Selection du prix et du nombre d'achat et date
                 
                 uiOutput("price"),
                 uiOutput("order_quantity"),
                 uiOutput("date"),
                 uiOutput("organic"),
                 uiOutput("name_product"),
                 
                 actionButton("apply_var", "Définir"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Jeu de données",
                 dataTableOutput("file_csv")),
        tabPanel("Prix au kilo",
                 downloadButton("download_mean_per_months", "Télécharger"),
                 dataTableOutput("prix_au_kilo"),
                 
                 h3("Jeu de données aberrantes"),
                 sliderInput("mult_EIQ", "De combien de fois l'EIQ doit on s'eloigner :", min = 0, max = 10, value = 1.5, step = 0.25),
                 dataTableOutput("prix_au_kilo_ab"),
                 dataTableOutput("dtout_vab"))
        
        
      ))
  ))


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
    
    dt <- read.csv(paste(path,"/", input$select_file, sep = ""), sep = input$sep, dec = input$dec) # on peut changer le chemin
    
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
  
  output$selct_var_kg <- renderUI({
    req(input$valide_file)
    
    dt <- data_select()
    choice <- names(dt)
    selectInput("var_kg", "Sélectionnez la vaiable de kg :", choices = choice, selected = NULL)
    
  })
  
  # Selection de la vairable prix
  
  output$price <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_price", "Sélectionnez la variable de prix :", choices = choices)
  })
  
  # Sélection du nombre d'achat du produit
  
  output$order_quantity <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_order_quantity", "Sélectionnez la variable du nombre de produit acheté :", choices = choices)
  })
  
  # Sélection de la date
  
  output$date <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_date", "Sélectionnez la variable date :", choices = choices)
  })
  
  # Selection var bio
  
  output$organic <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_organic", "Sélectionnez la variable de bio :", choices = choices)
  })
  
  # Séection du nom de produit
  
  output$name_product <- renderUI({
    req(data_select())
    
    choices <- names(data_select())
    
    selectInput("selected_product_name", "Sélectionnez le nom de produit :", choices = choices)
  })
  
  kg <- reactiveVal(NULL)
  
  order_qt <- reactiveVal(NULL)
  
  order_price <- reactiveVal(NULL)
  
  order_date <- reactiveVal(NULL)
  
  organic <- reactiveVal(NULL)
  
  name_product <- reactiveVal(NULL)
  
  # appliquer les variable de colonne
  
  observeEvent(input$apply_var, {
    
    kg(input$var_kg)
    
    order_qt(input$selected_order_quantity)
    
    order_price(input$selected_price)
    
    order_date(input$selected_date)
    
    organic(input$selected_organic)
    
    name_product(input$selected_product_name)
  })
  
  # calcul du prix au kilo (moyenne ponderee par mois)
  
  dt_vab <- reactiveVal(NULL)
  dt_months <- reactiveVal(NULL)
  dt_vab_m <- reactiveVal(NULL)
  
  output$prix_au_kilo <- renderDT({
    req(data_select())
    req(kg(), order_qt(), order_price(), order_date(), organic(), name_product())
    
    dt <- data_select()
    
    dt$pp_kilo <- dt[[order_price()]]/dt[[kg()]]
    
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
        moyenne_ponderee_totale = sum(pp_kilo * .data[[order_qt()]]*.data[[kg()]], na.rm = TRUE) /
          sum(.data[[order_qt()]]*.data[[kg()]], na.rm = TRUE),
        
        quantite_vendu_kg = sum(.data[[order_qt()]]*.data[[kg()]], na.rm = TRUE),
        
        moyenne_ponderee_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == TRUE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        quantite_vendu_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        moyenne_ponderee_non_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == FALSE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        quantite_vendu_non_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # CALCUL MOYENNE PONDEREE PAR MOIS bio/non bio/total pour données aberrantes
    
    dt_m_ab <- dt_nclean %>%
      group_by(.data[[name_product()]], month) %>%
      summarise(
        moyenne_ponderee_totale = sum(pp_kilo * .data[[order_qt()]]*.data[[kg()]], na.rm = TRUE) /
          sum(.data[[order_qt()]]*.data[[kg()]], na.rm = TRUE),
        
        quantite_vendu_kg = sum(.data[[order_qt()]]*.data[[kg()]], na.rm = TRUE),
        
        moyenne_ponderee_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == TRUE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        quantite_vendu_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == TRUE]*.data[[kg()]][as.logical(.data[[organic()]]) == TRUE], na.rm = TRUE),
        
        moyenne_ponderee_non_bio = sum(pp_kilo[as.logical(.data[[organic()]]) == FALSE] * .data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE) /
          sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
        quantite_vendu_non_bio_kg = sum(.data[[order_qt()]][as.logical(.data[[organic()]]) == FALSE]*.data[[kg()]][as.logical(.data[[organic()]]) == FALSE], na.rm = TRUE),
        
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
