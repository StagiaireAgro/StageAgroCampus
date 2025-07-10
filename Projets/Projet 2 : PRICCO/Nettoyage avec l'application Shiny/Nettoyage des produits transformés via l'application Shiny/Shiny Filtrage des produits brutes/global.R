
options(shiny.reactlog = TRUE)

# Installation & chargement des packages ----------------------------------

packages <- c(
  "shiny", "shinyWidgets", "data.table", "stringr", "stringi", 
  "tools", "dplyr", "yaml", "DT", "fs", "readxl")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) 
    install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# Configuration des chemins pour l'app ------------------------------------

config <- yaml::read_yaml("config/settings.yml")
output_dir <- config$paths$output_dir
data_dir <- config$paths$data_dir
log_file <- config$paths$log_file


# Enregistrer les noms des produits transformés rétirés -------------------

ajouter_prod_retirer <- function(log_file, nouvelle_entree) {
  if (!file.exists(log_file)) {
    file.create(log_file)
  }
  logs <- yaml::read_yaml(log_file)
  logs <- append(logs, nouvelle_entree, after = TRUE)
  write_yaml(logs, log_file)
}


# Chemin de tous les fichiers CSV dans le dossier des données ----------------

chemins_donnees <- dir_ls(path = data_dir, recurse = TRUE, glob = "*.xlsx")

# Fonction pour extraire le nom de la plateforme (2e dossier dans le chemin)
extraire_plateforme <- function(chemin) {
  strsplit(chemin, "/")[[1]][2]
}

# Générer une liste nommée des fichiers groupés par plateforme --------------

generer_liste_plateformes <- function(chemins) {
  plateformes <- sapply(chemins, extraire_plateforme)
  split(basename(chemins), plateformes)
}

# Liste des jeux de données groupés par plateforme ---------------------------

jeux_par_plateforme <- generer_liste_plateformes(chemins_donnees)

# Nettoyer les noms de produits pour obtenir les vrais niveaux -----------------

nettoyer_noms_produits <- function(noms) {
  noms <- str_replace_all(noms, c("â€™" = "'", "â€˜" = "'", "â€“" = "–", "â€”" = "—", "â€œ" = "\"", "â€" = "\"", "â€¦" = "…", "Ã©" = "é", "Ã¨" = "è", "Ãª" = "ê", "Ã " = "à", "Ã¢" = "â", "Ã¹" = "ù", "Ã»" = "û", "Ã´" = "ô", "Ã®" = "î", "Ã¤" = "ä", "Ã¶" = "ö", "Ã¼" = "ü", "ÃŸ" = "ß", "Ã†" = "Æ", "Ã¦" = "æ", "Ã˜" = "Ø", "Ã¸" = "ø", "Â" = "", "Â°" = "°", "Â€" = "€", "Â©" = "©", "Â®" = "®"))
  noms <- stri_trans_general(noms, "Latin-ASCII")  # enlever accents
  noms <- tolower(noms)
  noms <- trimws(noms)
  return(noms)
}

# Filtrer un jeu de données en retirant certains produits transformés ---------

filtrer_produits_transformes <- function(df, produits_a_exclure) {
  df[!df[["clean_name"]] %in% produits_a_exclure, ]
}
 




