# OUEDRAOGO Teeg-wendé Inoussa
# twinoussa55@gmail.com
# -------------------------------------------------------------------------

library(yaml)
library(vroom)
library(stringr)
library(stringi)


clean <- function(variable){
  x <- str_to_lower(variable)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[^a-z ]", "")
  return(x)
}

transform_in_regex <- function(mot) {
  mots <- str_split(mot, " ")[[1]]
  mots_regex <- sapply(mots, function(m) paste0(m, "(s)?"))
  return(paste0(mots_regex, collapse = ""))
}


search_product <- function(one_prod, ls_prod_ordered, ls_prod_regex){
  prod_find <- c()
  prod_tmp <- paste(unique(unlist(strsplit(one_prod, " "))), collapse = " ")
  for(indice in seq_along(ls_prod_regex)){
    pattern <- ls_prod_regex[indice]
    if(str_detect(prod_tmp, regex(pattern, ignore_case = TRUE))){
      prod_find <- c(prod_find, ls_prod_ordered[indice])
      prod_tmp <- str_replace(prod_tmp, regex(pattern, ignore_case = TRUE), "")
    }
  }
  return(paste(unique(prod_find), collapse = ","))
}



produits <- read_yaml("products.yaml")

# Socleo datasets ---------------------------------------------------------
path = "Data/Résultats données triées/socleo"
socleo_contents <- list.files(path, full.names = TRUE)

socleo_2021 <- vroom(socleo_contents[1])
socleo_2022 <- vroom(socleo_contents[2])
socleo_2023 <- vroom(socleo_contents[3])
socleo_2024 <- vroom(socleo_contents[4])







split_data <- function(socleo_2021, produits){
  liste_par_fruit <- list()
  # Nettoyage de la liste des mots produits
  clean_produits <- clean(produits) # miniscule, gestion accents & carac spéciaux
  clean_produits_sans_espaces <- gsub(" ", "", clean_produits) # Supp espace
  # Classer le regex et les mots de la liste en decroissant en fonction du vecteur retiré d'espace
  oder_clean_produits_sans_espaces <- order(nchar(clean_produits_sans_espaces), decreasing = TRUE) # Ordre
  # creation des regex de la liste des produits
  produits_regex <- sapply(clean_produits, transform_in_regex)
  # Ordonner de manière decroissante le regex
  produits_regex <- produits_regex[oder_clean_produits_sans_espaces]
  # Ordonner de manière decroissante la liste des produits
  clean_produits <- clean_produits[oder_clean_produits_sans_espaces]
  # Transformer la colonne name en facteur
  socleo_2021[["clean_name"]] <- as.factor(socleo_2021[["name"]])
  # Nettoyage des levels 
  levels(socleo_2021[["clean_name"]]) <- clean(levels(socleo_2021[["clean_name"]]))
  # Supprimer les espaces des levels
  levels(socleo_2021[["clean_name"]]) <- gsub("\\s+", "", levels(socleo_2021[["clean_name"]])) 
  levels_socleo_2021_ok <- levels(socleo_2021[["clean_name"]])
  classification <- sapply(levels_socleo_2021_ok, search_product, 
                           ls_prod_ordered = clean_produits, 
                           ls_prod_regex = produits_regex)
  socleo_2021[["classif"]] <- classification[as.character(socleo_2021[["clean_name"]])]
  socleo_2021[["clean_name"]] <- clean(as.factor(socleo_2021[["name"]]))
  socleo_2021[["classif"]][!is.na(socleo_2021[["classif"]]) & socleo_2021[["classif"]] == ""] <- NA
  
  
  # Dataset autre & propre
  data_ok <- socleo_2021[!is.na(socleo_2021[["classif"]]) & str_count(socleo_2021[["classif"]], ",") == 0, ]  
  data_pas_ok <- socleo_2021[is.na(socleo_2021[["classif"]]) | str_count(socleo_2021[["classif"]], ",") >= 1, ]
  
  for(fruit in clean_produits) {
    liste_par_fruit[[paste0("data_",fruit, sep="")]] <- 
      data_ok[data_ok[["classif"]] == fruit, ]
  }
  
  
  # Filtrer à nouveau data_pas_ok
  new_filter <- data_pas_ok[!is.na(data_pas_ok[["classif"]]),]
  
  levels_new_filter <- levels(as.factor(new_filter[["clean_name"]]))
  new_classification <- sapply(levels_new_filter, search_product, 
                          ls_prod_ordered = clean_produits, 
                          ls_prod_regex = produits_regex)
  
  new_filter[["classif"]] <- new_classification[new_filter[["clean_name"]]]
  levels_new_filter_prod <-  new_filter[str_count(new_filter[["classif"]], ",") == 0, "classif"]
  levels_new_filter_prod <- levels(as.factor(levels_new_filter_prod[["classif"]]))
  for(un_nom_produit in levels_new_filter_prod){
    liste_nom <- paste0("data_",un_nom_produit, sep="")
    new <- new_filter[new_filter[["classif"]]==un_nom_produit,]
    liste_par_fruit[[paste0("data_",un_nom_produit, sep="")]] <- rbind(
      liste_par_fruit[[paste0("data_",un_nom_produit, sep="")]],new)
    lignes_a_supprimer <- which(new_filter[["classif"]] == un_nom_produit)
    new_filter <- new_filter[-lignes_a_supprimer, ]
  }
  
  liste_par_fruit[["data_autre"]] <- rbind(new_filter, data_pas_ok[is.na(data_pas_ok[["classif"]]),])
  
  return(liste_par_fruit)
}

test <- split_data(socleo_2021, produits)



View(test)
View(test$data_autre)
View(test$data_boeuf)
sum(sapply(test, function(x) nrow(x)))


s <- test$data_autre

 

