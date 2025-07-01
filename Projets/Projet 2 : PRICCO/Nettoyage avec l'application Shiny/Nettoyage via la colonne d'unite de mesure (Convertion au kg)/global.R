# -------------------------------------------------------------------------

# twinoussa55@gmail.com
# +33 6 78 06 19 83
# 01-01-2025



# Packages (Installation & Chargement) ------------------------------------

packages <- c(
  "shiny", "shinyWidgets", "data.table", "stringr", "stringi", 
  "tools", "dplyr", "yaml", "DT", "fs", "readxl", "vroom")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) 
    install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# Organiser en liste les plateformes et leurs jeu de données --------------

extraire_plateforme <- function(chemin) {
  # Vérification qu'il existe un sous dossier
  parties <- path_split(chemin)[[1]]
  if (length(parties) > 2) return(parties[2])
  else return(parties[1])
}

generer_liste_plateformes <- function(chemins) {
  plateformes <- sapply(chemins, extraire_plateforme)
  split(basename(chemins), plateformes)
}

# Nettoyer les noms de produits -------------------------------------------

nettoyer_noms_produits <- function(noms) {
  noms <- str_to_lower(noms)                    
  noms <- stri_trans_general(noms, "Latin-ASCII") 
  noms <- str_replace_all(noms, "[^a-z ]", "") 
  noms <- trimws(noms)
  return(noms)
}

# Retourner un texte pour confirmer la validation du fichier selectionné --

success_select_file <- function(path, sep=" ; "){
  nom_plateforme <- path_split(path)[[1]][2]
  nom_jeu_donnees <- tools::file_path_sans_ext(basename(path))
  return(paste(nom_plateforme, nom_jeu_donnees, sep = sep))
}

# Configuration de l'application shiny ------------------------------------

config <- read_yaml("config/settings.yml")
ext_file <- config$extension_fichier$extension # Extension de fichier (ex : .csv)
data_dir <- config$chemins$dossier_data # Dossier des jeu de données
output_dir <- config$chemins$dossier_resultats # Dossier des sorties Shiny
CleanName <- config$data_cols$clean_name
ProductName <- config$data_cols$productName
ConditioningUnit <- config$data_cols$productConditioningUnit
ConditioningQuantity <- config$data_cols$productConditioningQuantity

# Chargement de l'ensemble chemins jeu de données -------------------------

data_path <- dir_ls(path = data_dir, recurse = TRUE, regexp = ext_file)
choix_db <- generer_liste_plateformes(data_path)

