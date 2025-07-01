fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    sidebarLayout(
        sidebarPanel(
            
            # Standardisation du conditionnement ------------------------------
            
            h3("Standardisation du conditionnement", 
               class = "text-primary font-weight-bold mb-3"),
            tags$hr(style = "border-top: 3px solid #4a90e2;"),
            h4("Jeu de données", class = "mt-4 mb-2 text-secondary"),
            uiOutput("list_file"),
            actionButton(
                "valide_file", "Validez le fichier sélectionné", 
                class = "btn btn-primary btn-block mt-3 mb-2"
            ),
            textOutput("sussess_import_db") %>% 
                tagAppendAttributes(class = "text-success font-italic mb-4"),
            uiOutput("ask_show_dataset"),

            # Conversion à partir des unités de mesure ----------------------

            h3("Conversion à partir des unités de mesure", 
               class = "text-primary font-weight-bold mb-3"),
            tags$hr(style = "border-top: 3px solid #4a90e2;"),
            selectInput(
                "var_unite_mesure", 
                "Sélectionnez la variable qui correspond aux unités de mesure :",
                choices = NULL
            ),
            actionButton(
                "valide_var_unite_mesure", "Appliquez la sélection",
                class = "btn btn-success btn-block mb-3"
            ),
            textOutput("sussess_valide_var_unite_mesure") %>% 
                tagAppendAttributes(class = "text-success font-italic mb-4"),
            uiOutput("ask_show_dataset_new"),
            br(),
            uiOutput("modalite_unite_mesure"),
            uiOutput("definir_taux_conversion"),
            uiOutput("convertion"),
            

            # Telecharger le jeu de données --------------------------------

            h3("Téléchargement du jeu de données", 
               class = "text-primary font-weight-bold mb-3"),
            tags$hr(style = "border-top: 3px solid #4a90e2;"),
            downloadButton(
                "download", 
                "Télecharger", 
                class = "btn btn-info btn-block"
            )
        ),
        mainPanel(
            tabsetPanel(id="onglets",
                tabPanel(
                    "Jeu de données", 
                    dataTableOutput("show_dataset")
                ),
                tabPanel(
                    "Conversion à partir des unités",
                    h4("Affectations actuelles :", 
                       class = "mt-4 mb-2 text-secondary"),
                    tableOutput("valeurs_associees"),
                    dataTableOutput("filtered_with_values")
                )
            )
        )
    )
)
