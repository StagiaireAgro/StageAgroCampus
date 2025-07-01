
function(input, output, session) {
    
    choix_actuel <- reactiveVal()
    dataset_calc <- reactiveVal()
    dataset_non_calc <- reactiveVal()
    dataset_calc_non_calc <- reactiveVal()
    current_dataset <- reactiveVal()
    valide_file_msg <- reactiveVal()
    values_unite_mesure <- reactiveVal()
    valide_unite_mesure_msg <- reactiveVal()
    valeurs_facteurs <- reactiveValues(data = list())
    var_unite_mesure_r <- reactiveVal()
        
    # Standardisation du conditionnement ------------------------------
    
    output$list_file <- renderUI(
        selectInput("select_file", "Sélectionnez un fichier", choices = choix_db)
    )
    
    observeEvent(input$valide_file, {
        req(input$select_file)
        path_data_sel <- data_path[basename(data_path) == input$select_file]
        dataset <- vroom::vroom(path_data_sel)
        dataset[[CleanName]] <- nettoyer_noms_produits(dataset[[ProductName]])
        success_msg <- success_select_file(path_data_sel)
        updateSelectInput(
            session, "var_unite_mesure", 
            choices = names(dataset),
            selected = ConditioningUnit
        )
        current_dataset(dataset)
        valide_file_msg(success_msg)
        valide_unite_mesure_msg(NULL)
    })
    
    output$sussess_import_db <- renderText({
        if (!is.null(current_dataset())) 
            paste("Fichier validé :", valide_file_msg())
        else "Aucun fichier sélectionné"
    })
    
    output$ask_show_dataset <- renderUI({
        req(current_dataset())
        switchInput(
            inputId = "show_option",
            label = "On/Off",
            value = FALSE,
            onLabel = "Afficher",
            offLabel = "Masquer",
            size = "small",
            onStatus = "success",  
            offStatus = "danger"   
        )
    })
    
    
    output$show_dataset <- renderDT({
        req(input$show_option)
        req(current_dataset())
        datatable(
            current_dataset(),
            options = list(
                scrollX = TRUE,
                scrollY = "calc(100vh - 150px)",
                pageLength = 25,
                autoWidth = TRUE
            ),
            width = "100%"
        )
    })
    
    # Conversion à partir des unités de mesure -----------------------
    
    observeEvent(input$valide_var_unite_mesure, {
        req(input$var_unite_mesure)
        unites <- unique(current_dataset()[[input$var_unite_mesure]])
        values_unite_mesure(sort(unites))
        choix_actuel(values_unite_mesure())
        var_unite_mesure_r(input$var_unite_mesure)
        valide_unite_mesure_msg(paste(
            "La variable qui correspond aux unités de mesure est :", 
            input$var_unite_mesure
        ))
        updateTabsetPanel(
            session, "onglets", 
            selected = "Conversion à partir des unités"
        )
    })
    
    output$sussess_valide_var_unite_mesure <- renderText({
        if(!is.null(input$valide_var_unite_mesure)) 
            valide_unite_mesure_msg()
    })
    
    
    output$ask_show_dataset_new <- renderUI({
        req(input$valide_var_unite_mesure)
        switchInput(
            inputId = "show_option_new",
            label = "On/Off",
            value = FALSE,
            onLabel = "Afficher",
            offLabel = "Masquer",
            size = "small",
            onStatus = "success",  
            offStatus = "danger"   
        )
    })
    
    output$modalite_unite_mesure <- renderUI({
        req(input$valide_var_unite_mesure)
        req(current_dataset())
        pickerInput(
            inputId = "select_modalites", 
            label = "Sélectionnez les unités de mesure à agréger :", 
            choices = values_unite_mesure(),
            selected = values_unite_mesure()[1],
            options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = FALSE,
                noneSelectedText = "Cochez une ou plusieurs unité(s) de mésure(s)",
            ), 
            multiple = TRUE
        )
    })
    
    output$valeurs_associees <- renderTable({
        req(input$valide_var_unite_mesure)
        assigned_values <- sapply(values_unite_mesure(), function(mod) {
            valeurs_facteurs$data[[mod]] %||% NA
        })
        data.frame(Modalité = values_unite_mesure(), Valeur = assigned_values)
    })
    
    output$definir_taux_conversion <- renderUI({
        req(input$valide_var_unite_mesure)
        req(current_dataset())
        numericInput(
            "taux_modalite_select", 
            "Définissez le taux de conversion ad hoc :", 
            value = NA
        )
    })
    
    output$convertion <- renderUI({
        req(input$valide_var_unite_mesure)
        req(current_dataset())
        actionButton(
            "convertir_unite_mesure", 
            "Appliquez la conversion", 
            class = "btn btn-success btn-block mb-3"
        )
    })
    
    observeEvent(input$convertir_unite_mesure, {
        req(input$select_modalites)
        req(!is.na(input$taux_modalite_select))
        valeurs_facteurs$data[input$select_modalites] <<- input$taux_modalite_select
        
        nouveaux_choix <- setdiff(choix_actuel(), input$select_modalites)
        choix_actuel(nouveaux_choix)
        
        updatePickerInput(
            session, 
            "select_modalites", 
            choices = nouveaux_choix,
            selected = if(length(nouveaux_choix) > 0) nouveaux_choix[1] else character(0)
        )
        updateNumericInput(
            session, 
            inputId = "taux_modalite_select", 
            value = NA
        )
    })
    
    output$filtered_with_values <- renderDT({
        req(var_unite_mesure_r())
        req(input$show_option_new)
        df <- current_dataset()
        df$modalite <- df[[var_unite_mesure_r()]]
        df$valeur_associee <- sapply(as.character(df$modalite), function(mod) {
            valeurs_facteurs$data[[mod]] %||% NA
        })
        df$kg_condit <- as.numeric(df[[ConditioningQuantity]])/df$valeur_associee
        dataset_calc(df[!is.na(df$valeur_associee),])
        dataset_non_calc(df[is.na(df$valeur_associee),])
        
        if (is.null(dataset_calc()) && is.null(dataset_non_calc())) 
            dataset_calc_non_calc(data.frame())  
        else if (is.null(dataset_calc())) 
            dataset_calc_non_calc(dataset_non_calc())
        else if (is.null(dataset_non_calc())) 
            dataset_calc_non_calc(dataset_calc())
        else dataset_calc_non_calc(rbind(dataset_calc(), dataset_non_calc()))

        DT::datatable(
            dataset_calc_non_calc(), #df[!is.na(df$valeur_associee),],
            options = list(
                scrollX = TRUE,
                scrollY = "calc(100vh - 150px)",
                pageLength = 25,
                autoWidth = TRUE
            ),
            width = "100%"
        )
    })
    
    output$download <- downloadHandler(
        filename = function() {
            req(dataset_calc_non_calc())
            parts <- unlist(strsplit(valide_file_msg(), " ; "))
            paste0(parts[2],"_filtre_par_unite_mesure", ".csv")
        },
        content = function(file) {
            parts <- unlist(strsplit(valide_file_msg(), " ; "))
            dossier <- parts[1]
            fichier <- paste0(parts[2], "_filtre_par_unite_mesure", ".csv")
            
            sous_dossier <- file.path(output_dir, dossier)
            dir.create(sous_dossier, recursive = TRUE, showWarnings = FALSE)
            
            write.csv(dataset_calc_non_calc(), file.path(sous_dossier, fichier), row.names = FALSE)
            write.csv(dataset_calc_non_calc(), file, row.names = FALSE)
        }
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
