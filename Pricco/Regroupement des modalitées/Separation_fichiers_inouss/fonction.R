
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
  prod_tmp <- one_prod
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
path = "data/socleo"
socleo_contents <- list.files(path, full.names = TRUE)

socleo_2021 <- vroom(socleo_contents[1])
socleo_2022 <- vroom(socleo_contents[2])
socleo_2023 <- vroom(socleo_contents[3])
socleo_2024 <- vroom(socleo_contents[4])

split_data <- function(socleo_2021, produits){
  liste_par_fruit <- list()
  # Nettoyage de la liste des mots produits
  clean_produits <- clean(produits) # miniscule, gestion accents & carac spéciaux
  clean_produits_sans_espaces <- gsub(" ", "|", clean_produits) # Supp espace
  # Classer le regex et les mots de la liste en decroissant en fonction du vecteur retiré d'espace
  oder_clean_produits_sans_espaces <- order(nchar(clean_produits_sans_espaces), decreasing = TRUE) # Du plus long au plus court mot
  produits_regex <- sapply(clean_produits, transform_in_regex) # creation des regex de la liste des produits
  produits_regex <- produits_regex[oder_clean_produits_sans_espaces] # Ordonner de manière decroissante le regex
  clean_produits <- clean_produits[oder_clean_produits_sans_espaces] # Ordonner de manière decroissante la liste des produits
  
  socleo_2021[["clean_name"]] <- as.factor(socleo_2021[["name"]]) # Transformer la colonne name en facteur
  levels(socleo_2021[["clean_name"]]) <- clean(levels(socleo_2021[["clean_name"]])) # Nettoyage des levels 
  levels(socleo_2021[["clean_name"]]) <- gsub("\\s+", "", levels(socleo_2021[["clean_name"]])) # Supprimer les espaces des levels
  
  # Filtre de dataset (eligible, non eligible)
  levels_pas_ok <- levels(socleo_2021[["clean_name"]])[!str_detect(levels(socleo_2021[["clean_name"]]), paste(clean_produits, collapse = "|"))]
  socleo_2021_pas_ok <- socleo_2021[socleo_2021[["clean_name"]] %in% levels_pas_ok,]
  socleo_2021_ok <- socleo_2021[!socleo_2021[["clean_name"]] %in% levels_pas_ok,]
  
  # 
  levels_socleo_2021_ok <- levels(socleo_2021_ok[["clean_name"]])
  classification <- sapply(levels_socleo_2021_ok, search_product, 
                            ls_prod_ordered = clean_produits, 
                            ls_prod_regex = produits_regex)
  socleo_2021_ok[["classif"]] <- classification[as.character(socleo_2021_ok[["clean_name"]])]
  
  
  for(fruit in clean_produits) {
    liste_par_fruit[[paste0("data_",fruit, sep="")]] <- 
      socleo_2021_ok[socleo_2021_ok[["classif"]] == fruit, ]
  }
  
  socleo_2021_pas_ok$classif <- NA
  
  liste_par_fruit[["data_autre"]] <- rbind(
    socleo_2021_pas_ok, socleo_2021_ok[str_count(socleo_2021_ok[["classif"]], ",") >= 1, ])
  
  return(liste_par_fruit)
}

s <- split_data(socleo_2021, produits)
ls(s)
