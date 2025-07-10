function(input, output, session) {
  
  # --- Variables réactives --------------------------------------------------
  data_select <- reactiveVal(NULL)           # Jeu de données sélectionné
  data_select_name <- reactiveVal(NULL)      # Nom complet (structure + fichier)
  noms_produits_unique <- reactiveVal(NULL)  # Liste unique des produits nettoyés
  
  vals_temp <- reactiveVal(NULL)              # Temporaire pour produits transformés
  retirer_prod <- reactiveVal(FALSE)
  
  
  # --- UI: Choix du fichier ------------------------------------------------
  output$file_list <- renderUI({
    selectInput("select_file", "Sélectionnez un fichier", choices = jeux_par_plateforme)
  })
  
  
  # --- Chargement du jeu de données ----------------------------------------
  observeEvent(input$valide_file, {
    req(input$select_file)
    
    path <- chemins_donnees[basename(chemins_donnees) == input$select_file]
    dt <- read_excel(path)
    dt[["clean_name"]] <- nettoyer_noms_produits(dt[["productName"]])
    
    structure_name <- strsplit(path, "/")[[1]][2]
    file_name <- tools::file_path_sans_ext(basename(path))
    valide_name <- paste(structure_name, file_name, sep = " ; ")
    
    data_select(dt)
    data_select_name(valide_name)
    
    # Mise à jour des produits uniques et PickerInput associés
    unique_products <- unique(dt[["clean_name"]])
    noms_produits_unique(unique_products)
    
    updatePickerInput(session, "levels", choices = sort(unique_products), selected = character(0))
    updatePickerInput(session, "levels_prod_trans", choices = character(0), selected = character(0))
    
    vals_temp(NULL)  # reset temporaire
  })
  
  
  # --- Affichage et confirmation --------------------------------------------
  output$file_valide <- renderText({
    if (!is.null(data_select())) paste("Fichier validé :", data_select_name(), "OK")
    else "Aucun fichier sélectionné"
  })
  
  output$afficher_datasets <- renderUI({
    req(data_select())
    switchInput(
      inputId = "afficher",
      label = "On/Off",
      value = FALSE,
      onLabel = "Afficher",
      offLabel = "Masquer",
      size = "small"
    )
  })
  
  output$file_csv <- renderDT({
    req(input$afficher)
    DT::datatable(
      data_select(),
      options = list(
        scrollX = TRUE,
        scrollY = "calc(100vh - 150px)",
        pageLength = 25,
        autoWidth = TRUE
      ),
      width = "100%"
    )
  })
  
  
  # --- Infos sur les niveaux de produits -----------------------------------
  output$nb_levels <- renderText({
    req(noms_produits_unique())
    paste("Nombre de levels : ", length(noms_produits_unique()))
  })
  
  output$nb_levels_selectionnes <- renderText({
    req(input$valide_file)
    paste("Nombre de noms de produits sélectionnés : ", length(input$levels))
  })
  
  output$nb_levels_produits_transformes <- renderText({
    req(input$valide_file)
    paste("Nombre de noms de produits transformés : ", length(input$levels_prod_trans))
  })
  
  
  # --- Synchronisation des sélections dans les PickerInputs ----------------
  observeEvent(input$levels, {
    req(input$levels)
    choix_tries <- sort(input$levels) #hm ça ne les trie pas ça en fait
    updatePickerInput(session, "levels_prod_trans", choices = sort(choix_tries), selected = choix_tries)
  })
  
  observe({
    vals_temp(input$levels_prod_trans)
  })
  
  observeEvent(input$valider_selections, {
    isolate({
      nouveaux_levels <- intersect(input$levels, vals_temp())
      updatePickerInput(session, "levels", selected = sort(nouveaux_levels))
    })
  })
  
  
  # --- Retirer produits transformés sélectionnés ----------------------------
  observeEvent(input$retirer_selections, {
    req(data_select())
    # Mettre à true si l'on a cliqué sur retirer_selectionné afin de permettre le telechargement
    retirer_prod(TRUE) 
    
    # Garder dans un fichier yml les produits considérés comme transformé
    
    if(!is.null(input$levels_prod_trans)){
      ajouter_prod_retirer(
        "log_produits_retires.yml", 
        input$levels_prod_trans)
    }
    
    # Filtrer les données en excluant les produits transformés
    data_select(
      filtrer_produits_transformes(data_select(), input$levels_prod_trans)
    )
    
    # Mise à jour des produits uniques et PickerInput
    updated_products <- unique(data_select()[["clean_name"]])
    noms_produits_unique(updated_products)
    
    updatePickerInput(session, "levels", choices = updated_products, selected = character(0))
    updatePickerInput(session, "levels_prod_trans", choices = character(0), selected = character(0))
    vals_temp(NULL)
  })
  
  
  # --- Téléchargement -------------------------------------------------------
  output$download <- downloadHandler(
    
    filename = function() {
      req(retirer_prod())
      parts <- unlist(strsplit(data_select_name(), " ; "))
      paste0(parts[2],"_filtre", ".csv")
    },
    content = function(file) {
      parts <- unlist(strsplit(data_select_name(), " ; "))
      dossier <- parts[1]
      fichier <- paste0(parts[2], "_filtre", ".csv")
      
      sous_dossier <- file.path(output_dir, dossier)
      dir.create(sous_dossier, recursive = TRUE, showWarnings = FALSE)
      
      dt_export <- data_select()[, !names(data_select()) %in% "clean_name", with = FALSE]
      write.csv(dt_export, file.path(sous_dossier, fichier), row.names = FALSE)
      write.csv(dt_export, file, row.names = FALSE)  # fichier temporaire pour l'utilisateur
    }
  )
  
}
