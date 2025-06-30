fluidPage(
  tags$style(HTML("
    @media (min-width: 768px) {
      .col-sm-4 {
        flex: 0 0 20% !important;
        max-width: 20% !important;
      }
      .col-sm-8 {
        flex: 0 0 80% !important;
        max-width: 80% !important;
      }
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h3("Sélection du jeu de données", class = "text-primary font-weight-bold mb-3"),
      tags$hr(style = "border-top: 3px solid #4a90e2;"),
      h4("Jeu de données", class = "mt-4 mb-2 text-secondary"),
      uiOutput("file_list"),
      actionButton("valide_file", "Validez le fichier sélectionné", class = "btn btn-primary btn-block mt-3 mb-2"),
      textOutput("file_valide") %>% tagAppendAttributes(class = "text-success font-italic mb-4"),
      uiOutput("afficher_datasets"),
      h3("Suppression des produits transformés", class = "text-primary font-weight-bold mt-5 mb-3"),
      tags$hr(style = "border-top: 3px solid #4a90e2;"),
      h4("Levels des noms des produits", class = "mt-3 mb-2 text-secondary"),
      pickerInput(
        inputId = "levels", 
        label = "Sélectionnez des noms de produits transformés", 
        choices = NULL,
        options = pickerOptions(
          actionsBox = FALSE,
          liveSearch = TRUE,
          noneSelectedText = "Sélectionnez un ou plusieurs noms de produit(s) pour les supprimer",
          size = 10
        ), 
        multiple = TRUE
      ),
      textOutput("nb_levels") %>% tagAppendAttributes(class = "text-muted small mt-1 mb-1"),
      textOutput("nb_levels_selectionnes") %>% tagAppendAttributes(class = "text-muted small mb-3"),
      br(),
      
      pickerInput(
        inputId = "levels_prod_trans", 
        label = "Liste des noms de produits sélectionnés", 
        choices = NULL,
        options = pickerOptions(
          actionsBox = FALSE,
          liveSearch = TRUE,
          noneSelectedText = "Désélectionnez le nom du produit s'il est brut",
          size = 10
        ), 
        multiple = TRUE
      ),
      textOutput("nb_levels_produits_transformes") %>% tagAppendAttributes(class = "text-muted small mb-3"),
      br(),
      
      actionButton("valider_selections", "Validez les choix", class = "btn btn-success btn-block mb-3"),
      actionButton("retirer_selections", "Retirez les produits transformés", class = "btn btn-danger btn-block mb-4"),
      
      h3("Téléchargement du jeu de données filtré", class = "text-primary font-weight-bold mb-3"),
      tags$hr(style = "border-top: 3px solid #4a90e2;"),
      downloadButton("download", "Télécharger", class = "btn btn-info btn-block")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Jeu de données",
                 dataTableOutput("file_csv"))
      )
    )
  )
)
