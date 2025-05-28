
# Le tidyverse est un méta-package qui regroupe plusieurs packages R
# utiles pour la manipulation, la transformation, et la visualisation de données :
# - ggplot2 : pour la visualisation
# - dplyr : pour la manipulation de données (filtrer, trier, grouper...)
# - tidyr : pour le reshaping (pivot_longer, pivot_wider, etc.)
# - readr : pour lire des fichiers plats (CSV, TSV...)
# - purrr : pour la programmation fonctionnelle (map, etc.)
# - tibble : pour manipuler des data frames modernes
# - stringr : pour manipuler des chaînes de caractères
# - forcats : pour manipuler des facteurs

library(tidyverse)

# Chargement de stringr pour le traitement de chaînes de caractères
library(stringr)

# Chargement de stringi pour manipulation avancée de texte (ex. suppression des accents)
library(stringi)

# readr permet une lecture rapide et efficace des fichiers plats (CSV, TSV, etc.)
library(readr)


# II) Importation des fichiers ################### 




########################## Socleo ########################## 

data_socleo_2021 <- read.csv("data/socleo/2021.csv")
data_socleo_2022 <- read.csv("data/socleo/2022.csv",sep = ";")
data_socleo_2023 <- read.csv("data/socleo/2023.csv",sep = ";")
data_socleo_2024 <- read.csv("data/socleo/2024.csv",sep = ";")

########################## coop circuit ########################## 

data_coop_circuit_2021 <- read.csv("data/coop circuit/par annee/2021.csv",sep = ",")
data_coop_circuit_2022 <- read.csv("data/coop circuit/par annee/2022.csv",sep = ";")
data_coop_circuit_2023 <- read.csv("data/coop circuit/par annee/2023.csv",sep = ";")
data_coop_circuit_2024 <- read.csv("data/coop circuit/par annee/2024.csv",sep = ";")

########################## la ruche qui dit oui ########################## 

data_ruche_oui_2021_part_1 <- read.csv("data/la ruche qui dit oui/historique_2021part1.csv",sep = ";")
data_ruche_oui_2021_part_2 <- read.csv("data/la ruche qui dit oui/historique_2021part1.csv",sep = ";")
data_ruche_oui_2022 <- read.csv("data/la ruche qui dit oui/historique_2022.csv",sep = ";")
data_ruche_oui_2023 <- read.csv("data/la ruche qui dit oui/historique_2023.csv",sep = ";")
data_ruche_oui_2024 <- read.csv("data/la ruche qui dit oui/historique_2024.csv",sep = ";")

# III) Définition de la liste de produits

# Liste des produits alimentaires à rechercher
produits <- c(
  "Jus de Poire", "Jus de pêche", "Abricot", "Banane", "Cerise", "Clémentine", "Fraise", "Kiwi", "Nectarine",
  "Noix", "Pêche", "Poireau", "Pomme de terre", "Jus de Pomme", "Pomme", "Prune",
  "Raisin", "Asperge", "Aubergine", "Carotte", "Céleri-branche", "Chou fleur",
  "Concombre", "Courgette", "Endive", "Haricot vert", "Laitue", "Melon",
  "Oignon", "Poire", "Poivron", "Potiron", "Tomate", "Lait", "Œuf", "Boeuf",
  "Porc", "Poulet", "Lentille", "Cassis", "Framboise", "Noisette", "Confiture de fraise", "Miel"
)

# IV) Fonction de tri des produits dans les données

# a) Fonction de nettoyage de texte

# Nettoie une chaîne de caractères : minuscules, sans accents, sans caractères spéciaux
clean <- function(variable){
  # Mise en minuscules
  x <- str_to_lower(variable)
  # Suppression des accents
  x <- stri_trans_general(x, "Latin-ASCII")
  # Suppression des caractères non alphabétiques sauf les espaces
  x <- str_replace_all(x, "[^a-z ]", "")
  return(x)
}

# b) Fonction pour transformer un produit en expression régulière flexible 

# Transforme un mot ou expression en une regex avec option pluriel sur chaque mot
transform_in_regex <- function(mot) {
  # Séparation des mots
  mots <- str_split(mot, " ")[[1]]
  # Ajoute une tolérance aux "s" (pluriel) pour chaque mot
  mots_regex <- sapply(mots, function(m) paste0(m, "(s)?"))
  # Combine en une seule regex
  return(paste0(mots_regex, collapse = ""))
}

# c) Fonction de détection de produit dans une ligne donnée 

# Recherche les produits reconnus dans une seule chaîne de caractère (one_prod)
search_product <- function(one_prod, ls_prod_ordered, ls_prod_regex){
  # Initialisation de la liste des produits trouvés
  prod_find <- c()
  # Supprime doublons et reconstitue une phrase unique
  prod_tmp <- paste(unique(unlist(strsplit(one_prod, " "))), collapse = " ")
  # Parcours de la liste des regex
  for(indice in seq_along(ls_prod_regex)){
    pattern <- ls_prod_regex[indice]
    # Si on détecte le pattern
    if(str_detect(prod_tmp, regex(pattern, ignore_case = TRUE))){
      # Ajouter le produit trouvé
      prod_find <- c(prod_find, ls_prod_ordered[indice])
      # Retirer le mot reconnu de la chaîne restante
      prod_tmp <- str_replace(prod_tmp, regex(pattern, ignore_case = TRUE), "")
    }
  }
  # Retourne les produits détectés, séparés par des virgules
  return(paste(unique(prod_find), collapse = ","))
}
                       

# d) Fonction principale de classification et de split des données

# Fonction principale pour classifier les données de socleo_2021 en fonction des produits
split_data <- function(socleo_2021, produits){
  
  # Initialisation de la liste de résultats
  liste_par_fruit <- list()
  
  # Nettoyage des noms des produits
  clean_produits <- clean(produits)
  
  # Suppression des espaces
  clean_produits_sans_espaces <- gsub(" ", "", clean_produits)
  
  # Tri des produits par longueur décroissante
  oder_clean_produits_sans_espaces <- order(nchar(clean_produits_sans_espaces), decreasing = TRUE)
  
  # Création des expressions régulières à partir des noms nettoyés
  produits_regex <- sapply(clean_produits, transform_in_regex)
  
  # Réorganisation des listes selon l’ordre décroissant
  produits_regex <- produits_regex[oder_clean_produits_sans_espaces]
  clean_produits <- clean_produits[oder_clean_produits_sans_espaces]
  
  # Transformation de la colonne "name" en facteur
  socleo_2021[["clean_name"]] <- as.factor(socleo_2021[["name"]])
  
  # Nettoyage des niveaux du facteur (minuscule, accents, etc.)
  levels(socleo_2021[["clean_name"]]) <- clean(levels(socleo_2021[["clean_name"]]))
  
  # Suppression des espaces dans les noms
  levels(socleo_2021[["clean_name"]]) <- gsub("\\s+", "", levels(socleo_2021[["clean_name"]])) 
  
  # Stocke les noms uniques nettoyés
  levels_socleo_2021_ok <- levels(socleo_2021[["clean_name"]])
  
  # Classification de chaque nom avec les produits
  classification <- sapply(levels_socleo_2021_ok, search_product, 
                           ls_prod_ordered = clean_produits, 
                           ls_prod_regex = produits_regex)
  
  # Ajoute la classification à la table
  socleo_2021[["classif"]] <- classification[as.character(socleo_2021[["clean_name"]])]
  
  # Nettoie à nouveau les noms nettoyés
  socleo_2021[["clean_name"]] <- clean(as.factor(socleo_2021[["name"]]))
  
  # Remplace les chaînes vides par NA
  socleo_2021[["classif"]][!is.na(socleo_2021[["classif"]]) & socleo_2021[["classif"]] == ""] <- NA
  
  
  # Séparation des données selon si une seule ou plusieurs catégories sont détectées
  data_ok <- socleo_2021[!is.na(socleo_2021[["classif"]]) & str_count(socleo_2021[["classif"]], ",") == 0, ]  
  data_pas_ok <- socleo_2021[is.na(socleo_2021[["classif"]]) | str_count(socleo_2021[["classif"]], ",") >= 1, ]
  
  # Création d’un sous-dataframe par fruit reconnu
  for(fruit in clean_produits) {
    liste_par_fruit[[paste0("data_",fruit, sep="")]] <- 
      data_ok[data_ok[["classif"]] == fruit, ]
  }
  
  # Tentative de reclassification des lignes ambigües
  new_filter <- data_pas_ok[!is.na(data_pas_ok[["classif"]]),]
  
  levels_new_filter <- levels(as.factor(new_filter[["clean_name"]]))
  
  # Reclassification
  new_classification <- sapply(levels_new_filter, search_product, 
                          ls_prod_ordered = clean_produits, 
                          ls_prod_regex = produits_regex)
  
  new_filter[["classif"]] <- new_classification[new_filter[["clean_name"]]]
  
  # Récupération des lignes maintenant bien classifiées
  levels_new_filter_prod <-  new_filter[str_count(new_filter[["classif"]], ",") == 0, "classif"]
  levels_new_filter_prod <- levels(as.factor(levels_new_filter_prod[["classif"]]))
  
  # Réintégration dans les bons sous-tableaux
  for(un_nom_produit in levels_new_filter_prod){
    liste_nom <- paste0("data_",un_nom_produit, sep="")
    new <- new_filter[new_filter[["classif"]] == un_nom_produit,]
    liste_par_fruit[[liste_nom]] <- rbind(
      liste_par_fruit[[liste_nom]], new)
    lignes_a_supprimer <- which(new_filter[["classif"]] == un_nom_produit)
    new_filter <- new_filter[-lignes_a_supprimer, ]
  }
  
  # Regroupe tout ce qui reste dans la catégorie "autre"
  liste_par_fruit[["data_autre"]] <- rbind(new_filter, data_pas_ok[is.na(data_pas_ok[["classif"]]),])
  
  return(liste_par_fruit)
}



#############################################################
#                                                           #
#   e) Tests de la fonction split sur différents fichiers   #
#                                                           #
############################################################# 

########################## Socleo ########################## 

liste_resultats_socleo_2021 <- split_data(data_socleo_2021,produits)
liste_resultats_socleo_2022 <- split_data(data_socleo_2022,produits)
liste_resultats_socleo_2023 <- split_data(data_socleo_2023,produits)
liste_resultats_socleo_2024 <- split_data(data_socleo_2024,produits)

########################## coop circuit ########################## 

liste_resultats_coop_circuit_2021 <- split_data(data_coop_circuit_2021,produits)
liste_resultats_coop_circuit_2022 <- split_data(data_coop_circuit_2022,produits)
liste_resultats_coop_circuit_2023 <- split_data(data_coop_circuit_2023,produits)
liste_resultats_coop_circuit_2024 <- split_data(data_coop_circuit_2024,produits)

########################## la ruche qui dit oui ########################## 

# On renomme la variable du nom de produit pour que la fonction marche
names(data_ruche_oui_2021_part_1)[8] <- c("name")

# On teste 
liste_resultats_ruche_oui_2021_part_1 <- split_data(data_ruche_oui_2021_part_1,produits)

# On renomme la variable du nom de produit pour que la fonction marche
names(data_ruche_oui_2021_part_2)[8] <- c("name")

# On teste 
liste_resultats_ruche_oui_2021_part_2 <- split_data(data_ruche_oui_2021_part_2,produits)

# On renomme la variable du nom de produit pour que la fonction marche
names(data_ruche_oui_2022)[8] <- c("name")

# On teste 
liste_resultats_ruche_oui_2022 <- split_data(data_ruche_oui_2022,produits)

# On renomme la variable du nom de produit pour que la fonction marche
names(data_ruche_oui_2023)[8] <- c("name")

# On teste 
liste_resultats_ruche_oui_2023 <- split_data(data_ruche_oui_2023,produits)

# On renomme la variable du nom de produit pour que la fonction marche
names(data_ruche_oui_2024)[8] <- c("name")

# On teste 
liste_resultats_ruche_oui_2024 <- split_data(data_ruche_oui_2024,produits)

###################################################################
#                                                                 #
#   Exemple d'accès à un sous-dataframe de la liste de résultat   #
#                                                                 #
################################################################### 

view(liste_resultats_socleo_2021$data_pomme)

view(liste_resultats_socleo_2022$data_autres)

view(liste_resultats_socleo_2023$data_autres)





