## ----message=FALSE---------------------------------------------------------------

# ---- Définir les packages nécessaires ----

required_packages  <- c(
  "vroom",       # lecture rapide de CSV/TSV
  "stringr",     # manipulation de chaînes
  "stringi",     # normalisation de chaînes (accents, etc.)
  "fs",          # gestion de fichiers
  "tidyverse",   # méta-package (ggplot2, dplyr, tidyr, etc.)
  "purrr",       # programmation fonctionnelle (déjà dans tidyverse)
  "writexl",     # sauvegarde de fichiers xlsx
  "tibble",      # tibbles modernes (déjà dans tidyverse)
  "ollamar",     # modèle de langage
  "pbapply",     # barre de progression pour apply
  "readxl",      # lecture de fichiers Excel
  "stringdist"   # distance de Levenshtein
  )

# Fonction pour vérifier et installer les packages
install_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      message(paste("Le package", pkg, "n'est pas installé. Tentative d'installation..."))
      install.packages(pkg, dependencies = TRUE)
      if (!require(pkg, character.only = TRUE)) {
        stop(paste("Impossible d'installer et de charger le package", pkg, ". Veuillez vérifier votre connexion internet ou les dépôts R."))
      }
    }
  }
}

install_and_load_packages(required_packages)



## ----message=FALSE---------------------------------------------------------------
# Définition du dossier racine contenant les fichiers CSV à importer
dossier_racine <- "data/2022-2023"

fichiers <- dir_ls(path = dossier_racine, recurse = TRUE, glob = "*.csv")
noms_fichiers <- path_ext_remove(path_file(fichiers))

datasets <- lapply(fichiers, function(file) vroom(file))

names(datasets) <- noms_fichiers


## --------------------------------------------------------------------------------
# Fonction pour ajouter une variable binaire de présence-absence pour des patterns

ajouter_colonne_binaire <- function(df, col_nom = "productName", new_col,
                                    detec_pattern = NULL, ab_pattern = NULL) {
  require(stringr)
  
  # Fonction de nettoyage des noms
  clean_name <- function(x) {
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- gsub("[^a-z0-9% ]", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  
  noms_prod <- df[[col_nom]]
  noms_prod <- factor(noms_prod)
  levels_prod <- levels(noms_prod)
  levels_prod_clean <- sapply(levels_prod, clean_name)
  
  # Initialisation du vecteur de détection
  solde_levels <- rep(0L, length(levels_prod_clean))
  
  # Détection avec pattern partiel (detec_pattern)
  if (!is.null(detec_pattern)) {
    pattern_partiel <- paste(detec_pattern, collapse = "|")
    solde_levels <- solde_levels | as.integer(str_detect(levels_prod_clean, pattern_partiel))
  }
  
  # Détection avec pattern exact (ab_pattern)
  if (!is.null(ab_pattern)) {
    # Ajouter délimiteurs de mot pour correspondance exacte
    pattern_exact <- paste0("\\b(", paste(ab_pattern, collapse = "|"), ")\\b")
    solde_levels <- solde_levels | as.integer(str_detect(levels_prod_clean, pattern_exact))
  }
  
  # Conversion logique -> entier binaire
  solde_levels <- as.integer(solde_levels)
  
  # Supprimer colonne si elle existe déjà
  if (new_col %in% names(df)) {
    df[[new_col]] <- NULL
  }
  
  # Ajouter la nouvelle colonne binaire
  df[[new_col]] <- solde_levels[as.integer(noms_prod)]
  
  return(df)
}


# On ajoute une colonne nommée 'productIsOrganic dans les dataset 'ruche_oui_2022' et 'ruche_oui_2023' pour indiquer si un produit est bio ou non

datasets$ruche_oui_2022 <- ajouter_colonne_binaire(datasets$ruche_oui_2022, col_nom = "product_name", new_col="productIsOrganic", detec_pattern=c("bio"), ab_pattern = c("ab", "AB", "Ab", "aB"))
datasets$ruche_oui_2023 <- ajouter_colonne_binaire(datasets$ruche_oui_2023, col_nom = "product_name", new_col="productIsOrganic", detec_pattern=c("bio"), ab_pattern = c("ab", "AB", "Ab", "aB"))


## --------------------------------------------------------------------------------
# Définition des colonnes à conserver pour les datasets provenant de Cagette
colonne_cagette_a_garder <- c(
  "orderProductPrice", "orderQuantity", "mois", "annee", "productName", 
  "productIsOrganic", "productConditioningQuantity", "productConditioningUnit", 
  "distributionZipCode"
)

# Identification des datasets dont le nom commence par "cagette"
indices_cagette <- startsWith(names(datasets), "cagette")

# Sélection des colonnes souhaitées pour chaque dataset de Cagette
datasets[indices_cagette] <- lapply(datasets[indices_cagette], function(x) {
  select(x, any_of(colonne_cagette_a_garder))
})

# Définition des colonnes à conserver pour les datasets de La Ruche Qui Dit Oui
colonne_ruche_a_garder <- c(
  "price_ttc_item", "nb_item", "mois", "annee", "product_name", "productIsOrganic",
  "weight_raw_item", "quantityunit", "hive_zipcode"
)

# Identification des datasets dont le nom commence par "ruche"
indices_ruche <- startsWith(names(datasets), "ruche")


# Sélection des colonnes souhaitées pour chaque dataset de La Ruche
datasets[indices_ruche] <- lapply(datasets[indices_ruche], function(x) {
  select(x, any_of(colonne_ruche_a_garder))
})

# Définition des colonnes à conserver pour les datasets de Socleo
colonne_socleo_a_garder <- c(
  "value", "quantite_com", "mois", "annee", "name", "is_organic",
  "quantite_cond", "unite_conditionnement", "code_postal"
)

# Identification des datasets dont le nom commence par "socleo"
indices_socleo <- startsWith(names(datasets), "socleo")

# Sélection des colonnes souhaitées pour chaque dataset de Socleo
datasets[indices_socleo] <- lapply(datasets[indices_socleo], function(x) {
  select(x, any_of(colonne_socleo_a_garder))
})

# Définition des colonnes à conserver pour les datasets de CoopCircuit
colonne_coop_circuit_a_garder <- c(
  "price", "quantite_unite", "mois", "annee", "name", "is_organic", 
  "quantite_cond", "conditionnement", "code_postal"
)

# Identification des datasets dont le nom commence par "coop_circuit"
indices_coop_circuit <- startsWith(names(datasets), "coop_circuit")

# Sélection des colonnes souhaitées pour chaque dataset de CoopCircuit
datasets[indices_coop_circuit] <- lapply(datasets[indices_coop_circuit], function(x) {
  select(x, any_of(colonne_coop_circuit_a_garder))
})

# Harmonisation des noms de colonnes pour toutes les plateformes autres que Cagette
# On renomme leurs colonnes pour correspondre à celles de Cagette
datasets[!indices_cagette] <- lapply(datasets[!indices_cagette], function(df) {
  df <- select(df, everything())  # Réorganise les colonnes si nécessaire
  colnames(df) <- colonne_cagette_a_garder  # Applique les noms standardisés
  return(df)
})

# Création d'une nouvelle liste contenant les datasets enrichis d'une colonne "plateforme"
# Cette colonne est extraite du nom de chaque dataset (ex: "ruche_oui_2022" --> "ruche")
liste_datasets <- lapply(names(datasets), function(nom) {
  df <- datasets[[nom]]
  df$plateforme <- str_split(nom, "_")[[1]][1]  # Extraction de la plateforme à partir du nom
  df
})

# Attribution des noms d’origine aux éléments de la liste finale
names(liste_datasets) <- names(datasets)

## --------------------------------------------------------------------------------
# Fusion des données de cagette

if ("cagette_Pomme_part_1_2023" %in% names(liste_datasets)){
  liste_datasets$cagette_Pomme_2022 <- rbind(
    liste_datasets$cagette_Pomme_part_1_2022,
    liste_datasets$cagette_Pomme_part_2_2022
  )
  
  liste_datasets$cagette_Pomme_2023 <- rbind(
    liste_datasets$cagette_Pomme_part_1_2023,
    liste_datasets$cagette_Pomme_part_2_2023
  )
  
  liste_datasets[["cagette_Pomme_2022"]] <- unique(liste_datasets[["cagette_Pomme_2022"]])
  liste_datasets[["cagette_Pomme_2023"]] <- unique(liste_datasets[["cagette_Pomme_2023"]])
  
}


## Sélection des dataframes par année
indices_cagette_2022 <- grepl("^cagette.*2022$", names(liste_datasets))
indices_cagette_2023 <- grepl("^cagette.*2023$", names(liste_datasets))

liste_datasets_cagette_2022 <- liste_datasets[indices_cagette_2022]
liste_datasets_cagette_2023 <- liste_datasets[indices_cagette_2023]

## Fusionner tous les dataframes en un seul par année
df_cagette_2022 <- do.call(rbind, liste_datasets_cagette_2022)
df_cagette_2023 <- do.call(rbind, liste_datasets_cagette_2023)

## Actualiser la liste : retirer tous les datasets "cagette"
liste_datasets_final <- liste_datasets[!indices_cagette]

## Ajouter les deux dataframes fusionnés à la liste
liste_datasets_final$cagette_2022 <- df_cagette_2022
liste_datasets_final$cagette_2023 <- df_cagette_2023

## --------------------------------------------------------------------------------
# Transformations des données de socleo
nettoyer_et_agreger <- function(df) {
  # Suppression des lignes avec orderQuantity ou orderProductPrice <= 0
  df <- df %>%
    filter(orderQuantity > 0,
           orderProductPrice > 0)
  
  df <- df %>%
    mutate(orderProductPrice = orderProductPrice / orderQuantity)
  
  return(df)
}


indices_socleo <- startsWith(names(liste_datasets_final), "socleo")
liste_datasets_final[indices_socleo] <- lapply(
  liste_datasets_final[indices_socleo],
  function(d) {
    if (!inherits(d, "data.frame")) {
      d <- as.data.frame(d)
    }
    nettoyer_et_agreger(d)
  }
)

## --------------------------------------------------------------------------------

# Fusion de tous les datasets (Ruche, Socleo, CoopCircuit, Cagette) en un seul data frame
combinaison <- do.call(rbind, liste_datasets_final)

# Ajout de la variable solde qui dit si un produit est déclassé ou pas
combinaison <- ajouter_colonne_binaire(combinaison, new_col="solde", detec_pattern = c("solde", "promo", "%", "declasse"))


## --------------------------------------------------------------------------------
# Liste des produits alimentaires à rechercher
liste_produits <- c(
  "Clémentine", "Tomate", "Carotte", "Cerise", "Chou fleur",
  "Fraise", "Kiwi", "Lait", "Oeuf", "Poireau", "Poire", "Raisin",
  "Haricot vert", "Noix", "Pomme", "Boeuf", "Porc", "Poulet",
  "Laitue", "Lentille", "Abricot", "Pomme de terre", "Oignon",
  "Courgette", "Aubergine", "Asperge", "Banane", "Poivron",
  "Jus de pomme", "Celeri branche", "Concombre", "Prune",
  "Peche", "Potiron", "Melon", "Endive", "Nectarine")

# Mise en minuscules pour correspondance plus facile
liste_produits <- str_to_lower(liste_produits)

# On supprime les tirets pour correspondance plus facile
liste_produits <- gsub("-", " ", liste_produits, ignore.case = TRUE)

# On supprime les accents pour correspondance plus facile
liste_produits <- stri_trans_general(liste_produits, "Latin-ASCII")

# On ordonne la liste des noms de produits de manière decroissante selon
# le nombre de caractere de chaque mots
ordre <- order(nchar(gsub(" ", "", liste_produits)), decreasing = TRUE)
liste_produits <- liste_produits[ordre]


## --------------------------------------------------------------------------------
# Nettoyer une chaine de caractères (-> ici liste de noms de produits)
nettoyer <- function(x) {
  # Convertit les caractères en minuscules et enlève les accents (ex: "É" devient "e")
  x <- stri_trans_general(str_to_lower(x), "Latin-ASCII")
  # Supprime tous les caractères non alphabétiques (hors a-z et espace)
  x <- str_replace_all(x, "[^a-z ]", "")
  # Supprime les espaces au début et à la fin de chaque chaîne
  x <- trimws(x)
  # Retourne le vecteur nettoyé
  return(x)
}

# Générer des expressions régulières à partir d'un mots ou liste de mots
creer_regex <- function(mots, suffixe = "(e?s)?", collapse = ".*") {
  # Applique la construction de regex à chaque mot ou expression
  sapply(mots, function(terme) {
    # Découpe l'expression en mots individuels
    tokens <- str_split(terme, " ")[[1]]
    # Ajoute le suffixe (par défaut : formes féminines/plurielles) à chaque mot
    regex_tokens <- paste0(tokens, suffixe)
    # Combine les morceaux avec '.*' entre eux pour matcher des variations textuelles
    paste0(regex_tokens, collapse = collapse)
  })
}

# Crée un fichier Excel standard pour permettre l'ajout d'autres déclinaisons
creer_fichier_excel <- function(liste_de_produits, fichier = "autres_declinaisons.xlsx") {
  classeur <- createWorkbook() # Crée un nouveau classeur Excel
  addWorksheet(classeur, "Ajoutez autres déclinaisons") # Ajoute une feuille nommée
  
  # Crée un tableau avec une colonne "Nom de produit" et une colonne vide "Autres déclinaisons"
  data <- data.frame(
    "Nom de produit" = liste_de_produits,
    "Autres déclinaisons" = "",
    check.names = FALSE
  )
  
  # Écrit le tableau dans la feuille Excel à partir de la première ligne avec les noms de colonnes
  writeData(
    classeur, 
    sheet = "Ajoutez autres déclinaisons", 
    x = data, 
    startRow = 1,
    colNames = TRUE
  )
  
  # Calcule le nombre total de lignes (données + en-tête) et de colonnes
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)
  
  # Calcule des largeurs de colonnes pour lisibilité (+5 caractères de marge)
  largeurs <- nchar(colnames(data)) + 5
  setColWidths(classeur, sheet = "Ajoutez autres déclinaisons", cols = 1:n_cols, widths = largeurs)
  
  # Crée un style pour l'en-tête (texte en gras et taille de police 12)
  header_style <- createStyle(textDecoration = "bold", fontSize = 12)
  addStyle(
    wb = classeur,
    sheet = "Ajoutez autres déclinaisons",
    style = header_style,
    rows = 1,
    cols = 1:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Crée un style déverrouillé pour permettre la modification des cellules (hors en-tête)
  unlocked_style <- createStyle(locked = FALSE)
  addStyle(
    wb = classeur,
    sheet = "Ajoutez autres déclinaisons",
    style = unlocked_style,
    rows = 2:n_rows,
    cols = 1:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Protège la feuille tout en autorisant la sélection des cellules verrouillées et déverrouillées
  protectWorksheet(
    wb = classeur,
    sheet = "Ajoutez autres déclinaisons",
    protect = TRUE,
    lockSelectingLockedCells = FALSE,
    lockSelectingUnlockedCells = FALSE
  )
  
  # Enregistre le fichier Excel sous le nom spécifié (en écrasant s’il existe)
  saveWorkbook(classeur, fichier, overwrite = TRUE)
}

# Fonction pour extraire toutes les déclinaisons des noms de produits à partir d'un dataset et d’un fichier externe facultatif
trouver_declinaisons_nom_produit <- 
  function(dataset, col_produit, ls_produit, regex, fichier = NULL) {
  # Nettoie les noms de produits dans la colonne spécifiée du dataset
  noms_des_produits <- nettoyer(dataset[[col_produit]])
  # Supprime les doublons parmi les noms nettoyés
  noms_des_produits_uniques <- unique(noms_des_produits)
  # Pour chaque expression régulière, extrait les correspondances dans les noms de produits uniques
  liste_declinaisons <- sapply(seq_along(regex), function(i) {
    extractions <- str_extract(noms_des_produits_uniques, regex[i]) # Applique la regex
    extractions <- extractions[!is.na(extractions) & extractions != ""] # Filtre les vides ou NA
    unique(extractions) # Supprime les doublons dans les extractions
  }, simplify = FALSE)
  
  # Donne à chaque élément de la liste le nom du produit correspondant
  names(liste_declinaisons) <- ls_produit
  
  # Si un fichier externe est fourni, ajouter les déclinaisons supplémentaires
  if(!is.null(fichier)==TRUE){
    fichier <- na.omit(fichier) # Supprime les lignes contenant des NA
    fichier[["Nom de produit"]] <- nettoyer(fichier[["Nom de produit"]]) # Nettoie les noms de produits du fichier
    sapply(fichier[["Nom de produit"]], function(prod){
      res <- pull(fichier[fichier[["Nom de produit"]]==prod,"Autres déclinaisons"]) # Récupère les déclinaisons associées au produit
      declinaisons_suppl <- unlist(str_split(res, ";")) # Sépare les déclinaisons par ";"
      liste_declinaisons[[prod]] <<- unique(c(liste_declinaisons[[prod]], declinaisons_suppl)) # Ajoute les déclinaisons à la liste existante en supprimant les doublons
    })
  }
  # Retourne la liste complète des déclinaisons
  return(liste_declinaisons)
}


## --------------------------------------------------------------------------------
# Creer les expressions reguliere de la liste des noms de produits
regex <- creer_regex(liste_produits) 
# Declinaison obtenues de manière automatique
final_declinaisons <- trouver_declinaisons_nom_produit(
    dataset = combinaison,
    col_produit = "productName",
    ls_produit = liste_produits,
    regex = regex
  )



## --------------------------------------------------------------------------------
# Fonction de séparation des fichiers par nom de produit
associer_noms_produits <- function(data, liste_produits, liste_declinai_noms_produits) {

  #### Étape 0 : Nettoyage du nom de produit
  data <- data %>%
    mutate(
      name_clean = productName %>%
        str_to_lower() %>%  # Mise en minuscules
        stri_trans_general("Latin-ASCII") %>%  # Suppression des accents
        gsub("-", " ", ., ignore.case = TRUE)  # Remplacement des tirets par des espaces
    )

  #### Étape 1 : Extraction des valeurs uniques dans `name_clean`
  levels_produits <- levels(as.factor(data$name_clean))

  #### Étape 2 : Création d’une matrice de correspondance entre noms et déclinaisons
  matrice_presence <- sapply(liste_declinai_noms_produits, function(declinaisons) {
    sapply(levels_produits, function(level) {
      any(str_detect(level, fixed(declinaisons))) * 1  # 1 si correspondance, 0 sinon
    })
  })

  # Transposition de la matrice pour avoir les produits en colonnes
  matrice_presence <- t(matrice_presence)

  # Calcul du nombre de correspondances pour chaque nom observé
  nb_variantes_noms <- colSums(matrice_presence)

  # Ajout d'une ligne résumant le nombre de correspondances par niveau
  matrice_presence <- rbind(matrice_presence, nb_variantes_noms)

  #### Étape 3 : Identification des noms qui correspondent à un seul produit
  levels_uniques <- names(nb_variantes_noms[nb_variantes_noms == 1])

  # On garde uniquement les colonnes de la matrice avec correspondance unique
  matrice_uniques <- matrice_presence[-nrow(matrice_presence), levels_uniques, drop = FALSE]

  #### Étape 4 : Création d’un vecteur de noms de produit bien identifiés
  vecteur_nom_produit <- sapply(levels_uniques, function(level) {
    idx <- which(matrice_uniques[, level] == 1)
    if (length(idx) == 1) {
      return(liste_produits[idx])
    } else {
      return(NA)
    }
  }, USE.NAMES = TRUE)

  # Suppression des cas NA (non identifiables de manière unique)
  vecteur_nom_produit <- vecteur_nom_produit[!is.na(vecteur_nom_produit)]

  # Création du data frame de correspondance nom nettoyé ↔ nom standard
  df_correspondance <- data.frame(
    name_clean = names(vecteur_nom_produit),
    nom_produit = unname(vecteur_nom_produit),
    stringsAsFactors = FALSE
  )

  #### Étape 5 : Création d’un sous-ensemble des données bien identifiées
  data_ok <- data %>%
    filter(name_clean %in% df_correspondance$name_clean) %>%
    left_join(df_correspondance, by = "name_clean")

  #### Étape 6 : Identification des noms ambigus ou non identifiés
  levels_pas_ok <- names(nb_variantes_noms[nb_variantes_noms != 1])

  # Création de la matrice pour les cas ambigus
  matrice_pas_ok <- matrice_presence[-nrow(matrice_presence), levels_pas_ok, drop = FALSE]

  # Création d’un vecteur listant tous les noms candidats pour chaque nom ambigus
  vecteur_nom_pas_ok <- sapply(levels_pas_ok, function(level) {
    idx <- which(matrice_pas_ok[, level] == 1)
    if (length(idx) == 0) {
      return(NA)
    } else {
      return(paste(liste_produits[idx], collapse = ";"))  # concatène les noms possibles
    }
  }, USE.NAMES = TRUE)

  # Data frame pour ces correspondances ambiguës
  df_correspondance_pas_ok <- data.frame(
    name_clean = names(vecteur_nom_pas_ok),
    nom_produit = unname(vecteur_nom_pas_ok),
    stringsAsFactors = FALSE
  )

  #### Étape 7 : Création du sous-ensemble avec noms ambigus ou multiples
  data_pas_ok <- data %>%
    filter(name_clean %in% df_correspondance_pas_ok$name_clean) %>%
    left_join(df_correspondance_pas_ok, by = "name_clean")

  #### Étape 8 : Extraction des lignes avec plusieurs correspondances (séparées par ;) 
  data_ambigus <- data_pas_ok %>%
    filter(str_detect(nom_produit, ";"))

  #### Étape 9 : Nettoyage des noms emboîtés pour éliminer les doublons inclus
  data_ambigus_clean <- data_ambigus %>%
    rowwise() %>%
    mutate(
      noms_list = list(str_split(nom_produit, ";")[[1]] %>% str_trim())  # on découpe et nettoie
    ) %>%
    mutate(
      noms_clean = list({
        noms <- noms_list
        # Supprime les noms qui sont contenus dans un autre
        noms[!sapply(noms, function(x) any(noms != x & str_detect(noms, fixed(x))))]
      })
    ) %>%
    mutate(
      nom_produit_new = ifelse(length(noms_clean) == 1, noms_clean[[1]], paste(noms_clean, collapse = ";"))
    ) %>%
    ungroup()

  #### Étape 10 : Extraire les lignes désormais associées à un seul nom
  data_deplaces <- data_ambigus_clean %>%
    filter(!str_detect(nom_produit_new, ";")) %>%  # gardes uniquement les cas désormais uniques
    select(-nom_produit, -noms_list, -noms_clean) %>%
    rename(nom_produit = nom_produit_new)

  #### Étape 11 : Mise à jour du dataset des correspondances uniques avec les lignes nettoyées
  data_ok <- bind_rows(data_ok, data_deplaces)

  #### Étape 12 : Suppression des lignes déplacées dans les données ambigües
  data_pas_ok <- data_pas_ok %>%
    filter(!(name_clean %in% data_deplaces$name_clean))

  # Retourne une liste avec les données bien identifiées et les autres
  return(list(data_ok = data_ok, data_pas_ok = data_pas_ok))
}


## ----message=FALSE,warning=FALSE-------------------------------------------------
# Application de la fonction d'identification des produits à partir des noms
# system.time() permet de mesurer le temps d'exécution du bloc de code
system.time({ 
  resultats_2 <- associer_noms_produits(
    data = combinaison,  # Le jeu de données fusionné (sans Cagette)
    liste_produits = liste_produits,  # Liste des noms de produits standardisés
    liste_declinai_noms_produits = final_declinaisons  # Liste des déclinaisons textuelles pour chaque produit
  )
})

# Récupération des lignes avec correspondance unique : noms de produit bien identifiés
data_prod_tries <- resultats_2$data_ok

# Récupération des lignes avec correspondances absentes ou ambiguës (plusieurs correspondances possibles)
data_prod_pas_tries <- resultats_2$data_pas_ok


## --------------------------------------------------------------------------------
# Suppression de la colonne temporaire 'name_clean' dans le jeu de données produit final (hors Cagette)
# Cette colonne servait au nettoyage/comparaison des noms de produits, mais n'est plus nécessaire

datasets_tts_plateformes_prod_tries <- data_prod_tries |> select(-c("name_clean"))
datasets_tts_plateformes_prod_pas_tries <- data_prod_pas_tries |> select(-c("name_clean"))


## --------------------------------------------------------------------------------

# Créer une colonne date au format "01-MM-YYYY"
datasets_tts_plateformes_prod_tries$mois <- as.numeric(datasets_tts_plateformes_prod_tries$mois)
datasets_tts_plateformes_prod_tries$date <- sprintf("01-%02d-%d", datasets_tts_plateformes_prod_tries$mois, datasets_tts_plateformes_prod_tries$annee)

## --------------------------------------------------------------------------------
recuperer_produits <- function(df, colonne="productName", produits_cibles, cible=TRUE) {
  require(stringdist)
  require(stringr)
  
  # Fonction de nettoyage des noms
  clean_name <- function(x) {
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- gsub("[^a-z0-9 ]", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x) 
  }
  
  # Fonctionpour remplacer les caractères spéciaux regex
  remplacer_caract_speci_regex <- function(x) {
    stringr::str_replace_all(x, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1")
  }
  
  noms_prod_df <- df[[colonne]]             # Récupérer la colonne
  noms_prod_df_uniques <- levels(factor(noms_prod_df))   # Récupérer les levels
  
  # Nettoyage des noms
  noms_df_clean <- tolower(sapply(noms_prod_df_uniques, clean_name))
  produits_cibles_clean <- tolower(sapply(produits_cibles, clean_name))
  
  # Matching par combinaison de mots
  matched_levels  <- sapply(seq_along(noms_df_clean), function(i) {
    produit <- noms_df_clean[i]
    any(sapply(produits_cibles_clean, function(ref) {
      mots <- strsplit(ref, "\\s+")[[1]]
      mots <- remplacer_caract_speci_regex(mots)
      pattern <- paste0(".*", paste(mots, collapse = ".*"), ".*")
      grepl(pattern, produit)
    }))
  })
  
  # Les levels a retourner
  if (cible) {
  noms_retour <- noms_prod_df_uniques[matched_levels]
  } else{
  noms_retour <- noms_prod_df_uniques[!matched_levels]
  }
  
  # Filtrer le data frame
  res <- df[noms_prod_df %in% noms_retour, , drop = FALSE]
  
  return(res)
}



## --------------------------------------------------------------------------------
# Liste de noms de référence
produits_cibles <- c("tomate cerise", "tomate boeuf", "laitue pomme", "tomate oeuf", "poivron tomate") 
#c("aubergine oeuf", "aubergine melongera", "carotte coeur boeuf", "cerise coeur boeuf", "chou fleur clémentine", "tomate clémentine", "boeuf joignon", "poire boeuf", "poivron corne boeuf", "tomate prune", "tomate raisin", "tomate poire", "tomate pomme")

#resultats <- recuperer_produits(data_prod_pas_tries_tts_platatefor, colonne = "productName", produits_cibles = produits_cibles)
#datasets_tts_plateformes_prod_tries <- rbind(datasets_tts_plateformes_prod_tries, resultats)

## --------------------------------------------------------------------------------



## --------------------------------------------------------------------------------
standardiser_unites <- function(data, col_source="productConditioningUnit") {
  
  # Nom de la colonne source pour la version standardisée
  col_std <- col_source
  
  # Nouveau nom pour garder l'ancienne version
  old_col <- paste0("old_", col_source)

  # Renomme l’ancienne colonne
  names(data)[names(data) == col_source] <- old_col
  
  
  # Création et copie la colonne source dans la colonne cible
  data[[col_std]] <- data[[old_col]]
  
  # Remplace les variantes de "kg" par "kg"
  data[[col_std]] <- gsub("\\b(Kg.|Kilogram)\\b", "kg", data[[col_std]], ignore.case = TRUE)
  
  # Remplace les variantes de "g" par "g"
  data[[col_std]] <- gsub("\\b(g.|Gram)\\b", "g", data[[col_std]], ignore.case = TRUE)
  
  # Remplace les variantes de "L" par "L"
  data[[col_std]] <- gsub("\\b(L.|Litre|ml.)\\b", "L", data[[col_std]], ignore.case = TRUE)
  
  # Remplace les variantes de "ml" par "ml"
  data[[col_std]] <- gsub("\\b(ml.)\\b", "ml", data[[col_std]], ignore.case = TRUE)
  
  # Remplace les variantes de "Piece" par "Piece"
  data[[col_std]] <- gsub("\\b(PiÃ¨ce|Pièce|Pièce|pce)\\b", "Piece", data[[col_std]], ignore.case = TRUE)
  
  return(data) 
}


## --------------------------------------------------------------------------------
levels(factor(datasets_tts_plateformes_prod_tries$productConditioningUnit))

## --------------------------------------------------------------------------------
datasets_tts_plateformes_prod_tries <- standardiser_unites(datasets_tts_plateformes_prod_tries, "productConditioningUnit")

## --------------------------------------------------------------------------------
cat("Unités de la colonne productConditioningUnit: ", "\n")
levels(factor(datasets_tts_plateformes_prod_tries$productConditioningUnit))
cat("\n")
cat("Unités de la colonne old_productConditioningUnit: ", "\n")
levels(factor(datasets_tts_plateformes_prod_tries$old_productConditioningUnit))


## --------------------------------------------------------------------------------
datasets_tts_plateformes_prod_tries <- na.omit(datasets_tts_plateformes_prod_tries)

# Récupérer les produits où l'unité correspond à Piece
datasets_tts_plateformes_prod_tries_unite_Pieces <- datasets_tts_plateformes_prod_tries %>% 
  filter(!is.na(productConditioningUnit) & str_detect(productConditioningUnit, "Piece"))

# Conserver les produits où l'unité est différente de Piece (c'est à dire les unités: kg, g, L, ...)
datasets_tts_plateformes_prod_tries <- setdiff(datasets_tts_plateformes_prod_tries, datasets_tts_plateformes_prod_tries_unite_Pieces)

# Conserver parmi les proquits où l'unité correspond à Piece, ceux qui ont des chiffres dans leur nom
datasets_tts_plateformes_prod_tries_unite_Pieces <-  datasets_tts_plateformes_prod_tries_unite_Pieces[str_detect(datasets_tts_plateformes_prod_tries_unite_Pieces$productName, "[0-9]"), ]

# Fusionner les données (produits pour lesquelles ont peu calculer le prix)
datasets_tts_plateformes_prod_tries <- union(datasets_tts_plateformes_prod_tries, datasets_tts_plateformes_prod_tries_unite_Pieces)

## --------------------------------------------------------------------------------
split_data_ok_par_produit <- function(data_ok) {
  # Vérifie que la colonne 'nom_produit' existe bien dans le data frame
  if (!"nom_produit" %in% colnames(data_ok)) {
    stop("La colonne 'nom_produit' est absente de data_ok")  # Arrête et affiche une erreur si la colonne manque
  }

  # Sépare le data frame en une liste de data frames, chaque sous-table correspondant à un produit unique
  liste_data_par_produit <- split(data_ok, data_ok$nom_produit)

  # Retourne cette liste de data frames
  return(liste_data_par_produit)
}




## --------------------------------------------------------------------------------
# Test de la fonction split_data_ok_par_produit sur le data frame global trié
liste_tables_37_produit_bruts <- split_data_ok_par_produit(datasets_tts_plateformes_prod_tries)


## --------------------------------------------------------------------------------

# Extraction des valeurs uniques de la colonne productName (noms bruts des produits)
levels_productName <- unique(datasets_tts_plateformes_prod_tries$productName)

# Extraction des valeurs uniques de la colonne nom_produit (noms produits standardisés)
levels_nom_produit <- unique(datasets_tts_plateformes_prod_tries$nom_produit)


# Fonction pour compter combien de noms bruts correspondent exclusivement à chaque produit standardisé
compter_occurrences_exclusives <- function(df, col_productName = "productName", col_nom_produit = "nom_produit") {
  
  # Étape 1 : récupérer tous les noms produits standardisés uniques, triés du plus long au plus court
  # Cela donne la priorité aux noms plus spécifiques lors de la recherche par motifs
  levels_nom_produit <- unique(df[[col_nom_produit]])
  levels_nom_produit <- levels_nom_produit[order(nchar(levels_nom_produit), decreasing = TRUE)]
  
  # Étape 2 : récupérer tous les noms bruts uniques (productName)
  all_product_names <- unique(df[[col_productName]])
  
  # Étape 3 : initialiser un vecteur logique pour indiquer si un nom brut a déjà été assigné à un produit
  noms_assignés <- rep(FALSE, length(all_product_names))
  names(noms_assignés) <- all_product_names
  
  # Étape 4 : pour chaque produit standardisé, compter les occurrences des noms bruts correspondants
  resultats <- map(levels_nom_produit, function(nom_produit) {
    
    # Créer un motif regex à partir des mots du nom produit, en gérant pluriels (s ou x) et séparateurs (espace ou tiret)
    mots <- str_split(nom_produit, " ")[[1]]
    mots_regex <- paste0("(", mots, ")(s|x)?")
    motif <- paste(mots_regex, collapse = "[- ]?")
    
    # Sélectionner uniquement les noms bruts encore non assignés
    names_disponibles <- all_product_names[!noms_assignés]
    
    # Compter le nombre d’occurrences du motif dans les noms disponibles (insensible à la casse)
    nb_occ <- str_count(tolower(names_disponibles), regex(motif, ignore_case = TRUE))
    
    # Sélectionner les noms bruts qui correspondent au motif
    matched_names <- names_disponibles[nb_occ > 0]
    matched_counts <- nb_occ[nb_occ > 0]
    
    # Marquer ces noms comme assignés pour éviter les doublons dans les prochains produits
    noms_assignés[matched_names] <<- TRUE
    
    # Retourner un tibble avec les noms bruts et le nombre d’occurrences pour ce produit
    tibble(
      !!paste0("levels_", str_replace_all(nom_produit, " ", "_")) := matched_names,
      !!paste0("nb_occurrence_", str_replace_all(nom_produit, " ", "_")) := matched_counts
    )
  }) |> set_names(levels_nom_produit)  # Nommer chaque élément de la liste par le produit correspondant
  
  return(resultats)
}


# Test de la fonction avec les listes extraites des données consolidées
liste_table_indi_purete_prod_trie <- compter_occurrences_exclusives(datasets_tts_plateformes_prod_tries)



## --------------------------------------------------------------------------------

ajouter_types_fin_produit <- function(df, col="productName", types_fin, autre_type = "classique") {
  require(stringr)
  
  # Fonction de nettoyage des noms
  clean_name <- function(x) {
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- gsub("[^a-z0-9 ]", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x) 
  }
  
  # Récupérer les levels de la colonne productName
  noms_prod_df <- factor(df[[col]])
  noms_prod_df_uniques <- levels(noms_prod_df)            

  # Nettoyer les niveaux
  noms_prod_df_uniques_clean <- sapply(noms_prod_df_uniques, clean_name)
  
  # Initialiser un vecteur de labels pour les niveaux, rempli par défaut
  labels_fin <- rep(autre_type, length(noms_prod_df_uniques))
  
  
  # Pour chaque type, on attribue ce label aux niveaux correspondants
  for (type in types_fin) {
    indices <- str_detect(noms_prod_df_uniques_clean, regex(tolower(type), ignore_case = TRUE))
    labels_fin[indices] <- type
  }
  
  df$type_fin_produit <- labels_fin[as.integer(noms_prod_df)]
  
  return(df)
}


## --------------------------------------------------------------------------------

liste_levels_37_produit <- lapply(liste_tables_37_produit_bruts, function(x){data.frame("nom_produit" = x$nom_produit[1],"levels" = sample(levels(factor(x$productName))))} )



## --------------------------------------------------------------------------------
# On génère les prompts et on applique Gemma dessus
detecter_levels_prod_transfor <- function(liste_levels, taille_chunck) {
  
  nom_prod <- liste_levels$nom_produit[1]
  
  total_levels <- length(liste_levels$levels)
  nb_chunck <- floor(total_levels/taille_chunck) + 1

  # Création des chunks de taille `taille_chunck` avec gestion du dernier chunk partiel
  chunks <- split(liste_levels$levels, ceiling(seq_along(liste_levels$levels) / taille_chunck))

  # Nombre total de chunks générés
  total_chunks <- length(chunks)

  # Limiter au nb_chunck demandé, mais sans dÃ©passer ceux disponibles
  chunks <- chunks[1:min(nb_chunck, total_chunks)]

  # Prompt de base (dynamique en fonction du produit)
base_prompt <- paste0(
  "Tu es un expert en alimentation et en classification des produits alimentaires.",
  "Tu reçois une liste de produits à base de ", nom_prod, ". ", 
  "Ta tâche est d’**identifier uniquement les produits transformés** dans cette liste.",
  "---",
  "### \uD83D\uDD36 Définition d’un **produit transformé** :",
  "Un produit est considéré comme **transformé** s’il remplit **au moins un** des critères suivants :",
  "- Il contient **d’autres ingrédients en plus de la ", nom_prod, "** (par exemple : chou, vinaigrette, gingembre, panais, quinoa, etc.)",
  "- Il a subi une **transformation culinaire** (ex. cuit, râpé, assaisonné, mixé, fermenté, stérilisé…)",
  "- Il est présenté ou conditionné comme un **plat préparé** (ex. soupe, purée, tartinade, chips, coleslaw, conserve, plat cuisiné, bocal, barquette, bouteille…)",
  "- Il est destiné à un **usage spécifique** (ex. apéritif, snacking, bébé, mix de crudités, etc.)",
  "---",
  "### ⛔ À l’inverse, un **produit brut** est **non transformé** :",
  "Tu dois **ignorer tous les produits bruts**, même s’ils sont variés ou bio.",  
  "Un produit est brut s’il :",
  "- est uniquement composé de **", nom_prod, "**, sans aucun autre ingrédient",
  "- est présenté sous forme **naturelle** : entière, en botte, en vrac, en sac, lavée ou non, bio ou non, orange, jaune, violette, de différentes variétés",
  "- n’a **subi aucune transformation culinaire**",
  "- ne contient **aucune mention de préparation, de mélange ou de conditionnement culinaire**",
  "\uD83D\uDC49 Exemples de produits **bruts à ignorer** :  ",
  paste0(nom_prod, ", ", nom_prod, " vrac, ", nom_prod, " lavées, ", nom_prod, " bio, ", nom_prod, " en botte, ", nom_prod, " jaune, ", nom_prod, " non lavées, etc."),
  "---",
  "### \uD83D\uDCE4 Ce que tu dois faire :",
  "- **Retourne uniquement** les **noms exacts** des produits transformés de la liste, **copiés tels quels**",
  "- N’ajoute **aucune explication, aucun commentaire**",
  "- Rends la sortie **exclusivement** au format R suivant :",
  "c(\"produit transformé\", \"produit transformé\", \"produit transformé\")",
  "Voici la liste de produits à analyser :"
)



  # Générer les prompts complets en y incorporant les levels
  prompts <- lapply(chunks, function(chunk) {
    paste0(base_prompt,paste(chunk, collapse = "\n"))
    
  })

  # Générer les produits transformés en appelant Gemma3 pour chaque prompt
  # resultats <- lapply(prompts, function(p) {
  #   generate("gemma3", p, stream = FALSE, output = "text")
  # })

  # Résultats des prompts et de Gemma 3
  #return(list(prompts = prompts, resultats = resultats))
  
  # Résultats des prompts
  return(prompts = prompts)
}


## --------------------------------------------------------------------------------
###### On génère les prompts de taille 60 pour les 37 produits

liste_prompts_37_produit <- lapply(liste_levels_37_produit, function(x) {detecter_levels_prod_transfor(liste_levels = x, taille_chunck = 60)})




## --------------------------------------------------------------------------------

requete_ollama <- function(prompts){
  start_time <- Sys.time()
  
  # Application avec barre de progression
  res <- pblapply(prompts, function(prompt) {
  generate("llama3.1", prompt, stream = FALSE, output = "text")
    })
  
  # Timer de fin
  end_time <- Sys.time()
  print(end_time - start_time)
  
  return (res)
}



## --------------------------------------------------------------------------------

prod_select <- c("abricot", "poireau")


liste_prompts_produits_select <- liste_prompts_37_produit[prod_select]



## --------------------------------------------------------------------------------

liste_res_llm <- lapply(liste_prompts_produits_select, function(x) {requete_ollama(x)})




## --------------------------------------------------------------------------------
# Fonction permettant d'extraire une liste de produits transformés depuis un texte généré par un LLM

extraire_liste_produits <- function(texte_source) {
  # Étape 1 : Remplace les appels à 'C(' par 'c(' (R est sensible à la casse, 'C(' pourrait causer une erreur)
  texte_source <- gsub("C\\(", "c(", texte_source, ignore.case = TRUE)

  # Étape 2 : Supprime les sauts de ligne pour éviter de casser la chaîne de caractères lors de l'évaluation
  texte_source <- gsub("\n", " ", texte_source)

  # Étape 3 : Définit le motif (regex) pour détecter un appel à la fonction R c(...) contenant une liste de chaînes
  motif_vecteur <- "c\\s*\\((.*?)\\)"

  # Étape 4 : Cherche la première correspondance avec le motif défini ci-dessus
  correspondance_vecteur <- regmatches(texte_source, regexpr(motif_vecteur, texte_source, perl = TRUE))

  # Étape 5 : Si une correspondance a été trouvée et qu’elle n’est pas vide, tenter de l’évaluer comme du code R
  if (length(correspondance_vecteur) > 0 && correspondance_vecteur != "") {
    tentative_evaluation <- tryCatch(
      eval(parse(text = correspondance_vecteur)),  # Essaie d'exécuter le texte comme du code R
      error = function(e) NULL  # En cas d'erreur, retourne NULL
    )

    # Étape 6 : Si l’évaluation a réussi et retourne un vecteur de chaînes de caractères, le retourner
    if (!is.null(tentative_evaluation) && is.character(tentative_evaluation)) {
      return(tentative_evaluation)
    }
  }

  # Étape 7 : Si aucun vecteur c(...) exploitable n’a été trouvé, chercher manuellement des chaînes entre guillemets
  positions_chaines <- gregexpr('"([^"]{2,100})"', texte_source, perl = TRUE)
  chaines_trouvees <- regmatches(texte_source, positions_chaines)[[1]]

  # Étape 8 : Si au moins deux chaînes sont extraites, les retourner comme vecteur après nettoyage
  if (length(chaines_trouvees) >= 2) {
    return(trimws(gsub('"', "", chaines_trouvees)))  # Supprime les guillemets et espaces superflus
  }

  # Étape 9 : Vérifie si le texte ressemble à une liste (éléments précédés de * ou -)
  if (grepl("\\*\\s+", texte_source) || grepl("-\\s+", texte_source)) {
    lignes_separees <- unlist(strsplit(texte_source, "\n"))  # Découpe le texte ligne par ligne
    lignes_avec_puce <- grep("^\\s*(\\*|-)", lignes_separees, value = TRUE)  # Ne garde que les lignes avec puces

    if (length(lignes_avec_puce) > 0) {
      return(trimws(gsub("^(\\*|-)+\\s*", "", lignes_avec_puce)))  # Supprime les puces et espaces initiaux
    }
  }

  # Étape 10 : Si aucune méthode n’a permis d’extraire une liste, retourner NULL
  return(NULL)
}



## --------------------------------------------------------------------------------
comparer_listes_produits <- function(prompts_attendus, reponses_llm) {
  
  # Étape 1 : Extraction des produits attendus depuis les prompts (niveau "level")
  produits_attendus <- lapply(prompts_attendus, function(texte_prompt) {
    # Extrait tout ce qui suit "Voici la liste de produits à analyser :"
    texte_extrait <- str_extract(
      texte_prompt,
      "(?s)(?<=Voici la liste de produits à analyser :)\\s*.*"
    )
    # Découpe le texte extrait ligne par ligne
    unlist(strsplit(texte_extrait, "\n"))
  })
  
  # Étape 2 : Extraction des produits reconnus par le LLM (via la fonction extraire_liste_produits defini plus haut)
  produits_extraits_llm <- lapply(reponses_llm, function(texte_reponse) {
    extraire_liste_produits(texte_reponse)
  })
  
  # Fonctionpour remplacer les caractères spéciaux regex
  remplacer_caract_speci_regex <- function(x) {
    stringr::str_replace_all(x, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1")
  }
  
    # Fonction pour nettoyer les noms
  clean_name <- function(x) {
    stringr::str_replace_all(x, "[^a-zA-Z0-9 ]", "")
    }

  # Étape 3 : Comparaison entre les produits attendus et extraits
  comparaison <- lapply(seq_along(produits_attendus), function(i) {
    liste_attendue <- produits_attendus[[i]]
    liste_extraite <- produits_extraits_llm[[i]]
    
    # Identifie les produits présents dans les deux listes (correspondance exacte)
    produits_communs <- intersect(liste_attendue, liste_extraite)
    
    # Identifie les "hallucinations" brutes ou pseudo_hallucinations : produits extraits par le LLM mais absents de la liste initiale 
    pseudo_hallucinations <- setdiff(liste_extraite, liste_attendue)
    
    # Identifie les hallucinations réelles : toutes les pseudo_hallucinations qui ne sont pas des nom de produits mal écrits par le modèle, c'est ceux qu'il a inventé
    # Pour cela on construit une expression régulière avec chaque mot ou élément du nom de produit renvoyé par le modèle () et on va recherché si il y existe des vrai noms de produits qui sont composé de l'intégralité de ces mots, si non alors on considère que ces noms de produit ont été inventé par le modèle (hallucinations_reelles)

    hallucinations_reelles <- pseudo_hallucinations[!sapply(pseudo_hallucinations, function(h) {
      #  on nettoie le nom du produits et on le découpe à partir des espaces
      mots <- strsplit(tolower(clean_name(h)), "\\s+")[[1]] 
      mots <- remplacer_caract_speci_regex(mots)
      
      if (length(mots) == 0) return(FALSE)  # cas vide (pas de hallucination)
      
      # construction du regex : .*mot1.*mot2.*mot3.*
      pattern <- paste0(".*", paste(mots, collapse = ".*"), ".*")
      any(grepl(pattern, tolower(clean_name(liste_attendue))))
    })]
    
    # on retir les vrais hallucinations des pseudo_hallucinations (vrai nom de produit mal écrit)
    pseudo_hallucinations <- setdiff(pseudo_hallucinations, hallucinations_reelles)
    
    # on va rechercher la vrai écriture des pseudo_hallucinations pour corriger l'orthographe des réponses du modèle
    hallucinations_corrige <- unlist(lapply(pseudo_hallucinations, function(h) {
  mots <- strsplit(tolower(clean_name(h)), "\\s+")[[1]]
  mots <- remplacer_caract_speci_regex(mots)
  if (length(mots) == 0) return(NULL) 
  
  pattern <- paste0(".*", paste(mots, collapse = ".*"), ".*")
  
  # on récupère les vrais noms de produit qui match avec l'expression régulière
  vrais_noms <- liste_attendue[sapply(liste_attendue, function(p) {
    grepl(pattern, tolower(clean_name(p)))
  })]
  
  if (length(vrais_noms) == 0) return(NULL) # Cas vide
  
  # Si un seul match, on le retourne directement
  if (length(vrais_noms) == 1) return(vrais_noms)
  
  # Sinon il y a plusieurs noms de produit, dans ce cas on va chercher à retourner le vrai nom de produit qui est le plus proche de celui renvoyé par le modèle 
  
  # rechercher une correspondance exacte (en minuscule)
  nom_llm <- tolower(h)
  correspondance_exacte <- vrais_noms[tolower(vrais_noms) == nom_llm]
  
  if (length(correspondance_exacte) == 1) {
    return(correspondance_exacte)
  } else {
    # On choisit celui avec la plus petite distance de Levenshtein (vrai nom de produit qui est le plus proche de celui renvoyé par le modèle)
    distances <- stringdist(nom_llm, tolower(vrais_noms), method = "lv")
    meilleur_nom <- vrais_noms[which.min(distances)]
    return(meilleur_nom)
  }
}))
    # vérifier qu'on a bien des levels uniques
    hallucinations_corrige <- unique(hallucinations_corrige)
    
    # On corrige les réponses du modèle en rajoutant les vrais noms de produit dans les Levels communs s’ils n’existent pas déjà (unique)
    produits_communs <- unique(c(produits_communs, hallucinations_corrige))
    
    prod_transfo_levels_initiaux <- NA
    produit_transfo_llm <- produits_communs
    prod_transfo_pas_trouver <- NA
    
    # Structure des résultats par comparaison
    list(
      levels_initiaux = liste_attendue,
      levels_llm = liste_extraite,
      fausses_hallucina = pseudo_hallucinations,
      vraies_hallucina = hallucinations_reelles,
      levels_communs = produits_communs,
      prod_transfo_attendus = prod_transfo_levels_initiaux,
      produit_transfo_llm = produit_transfo_llm,
      prod_transfo_pas_trouver = prod_transfo_pas_trouver
    )
  })
  
  # Étape 4 : Retourne la liste complète des résultats de comparaison
  return(comparaison)
}


## --------------------------------------------------------------------------------
# fonction pour construire le tableau d'indicateur
table_indicateurs_purete <- function(resultats_comparaison){
  
  # On initialise le tableau d'indicateur de pureté 
  table_indica_purete <- data.frame(
    chunck_traite = c(1:length(resultats_comparaison)),
    nb_levels_initial = NA,
    nb_levels_llm = NA,
    nb_levels_commun = NA,
    nb_fausses_halluci_llm = NA,
    nb_vraies_halluci_llm = NA,
    nb_prod_attendus = NA, 
    nb_prod_transfo_trouves_llm = NA,
    nb_prod_transfo_pas_trouves_llm = NA,
    nb_prod_llm_st_prod_bruts = NA)

  # On remplit la colonne du nb_levels_initial
  table_indica_purete$nb_levels_initial <- sapply(resultats_comparaison, function(x) length(x$levels_initiaux))
  
  # On remplit la colonne du nb_levels_llm
  table_indica_purete$nb_levels_llm <- sapply(resultats_comparaison, function(x) length(x$levels_llm))

  # On remplit la colonne du nb_levels_commun
  table_indica_purete$nb_levels_commun <- sapply(resultats_comparaison, function(x) length(x$levels_communs))
  
  # On remplit la colonne du nb_fausses_halluci_llm
  table_indica_purete$nb_fausses_halluci_llm <- sapply(resultats_comparaison, function(x) length(x$fausses_hallucina))
  
  # On remplit la colonne du nb_vraies_halluci_llm
  table_indica_purete$nb_vraies_halluci_llm <- sapply(resultats_comparaison, function(x) length(x$vraies_hallucina))
  
  # On remplit la colonne du nb_prod_attendus
  table_indica_purete$nb_prod_attendus <- if(!any(is.na(resultats_comparaison[[1]]$prod_transfo_attendus))){sapply(resultats_comparaison, function(x) length(x$prod_transfo_attendus))}
  
  # On remplit la colonne du nb_prod_transfo_trouves_llm
  table_indica_purete$nb_prod_transfo_trouves_llm <- sapply(resultats_comparaison, function(x) length(x$produit_transfo_llm))
  
  # On remplit la colonne du nb_prod_transfo_pas_trouves_llm
  table_indica_purete$nb_prod_transfo_pas_trouves_llm <- if(!any(is.na(resultats_comparaison[[1]]$prod_transfo_pas_trouver))){sapply(resultats_comparaison, function(x) length(x$prod_transfo_pas_trouver))}
  
  # On remplit la colonne du nombre de produit llm qui sont des produits bruts
  table_indica_purete$nb_prod_llm_st_prod_bruts <- if(!any(is.na(resultats_comparaison[[1]]$prod_transfo_pas_trouver))){sapply(resultats_comparaison, function(x) (length(x$levels_communs) - length(x$produit_transfo_llm)))}
  
  
  
  ### Calcul des moyennes et totaux
  
  # Selectionner les noms des colonnes en retirant la colonne "chunck_traite" 
  cols_to_sum <- names(table_indica_purete)[-1]
  
  # Calcul des totaux et moyennes par colonne
  totaux <- colSums(table_indica_purete[, cols_to_sum], na.rm = TRUE)
  moyennes <- colMeans(table_indica_purete[, cols_to_sum], na.rm = TRUE)
  
  # Création des lignes à ajouter (ligne Total et Moyenne) et ajout de la colonne "chunck_traite"
  ligne_total <- c(chunck_traite = "Total", round(totaux))
  ligne_moyenne <- c(chunck_traite = "Moyenne", round(moyennes))
  
  # Ajout au tableau 
  table_indica_purete <- rbind( 
    table_indica_purete, ligne_total, ligne_moyenne)
  
  # Reconversion des colonnes sauf 'chunck_traite' en numérique
  table_indica_purete[cols_to_sum] <- lapply(table_indica_purete[cols_to_sum], as.numeric)
  
  # On remplace la moyenne de levels_initial par un NA car ça n'a pas de sens
  table_indica_purete[length(resultats_comparaison)+2,2] <- NA
  
  return(table_indica_purete)
}

## --------------------------------------------------------------------------------
# Fonction de suppresion des levels de produits transformés
retirer_prod_transfo <- function(data, res_comparaison, stat=TRUE){
  
  # Récupérer toute las liste des produits transformés trouvés par le llm
  liste_prod_transfo_llm <- unlist(lapply(res_comparaison, function(x) x$produit_transfo_llm))
  
  # On vérifie que les levels de produits transformés sont bien uniques
liste_prod_transfo_llm <- unique(liste_prod_transfo_llm)

  # On supprime les produits transformés
  data_bruts <- data[!(data$productName %in% liste_prod_transfo_llm), ]
  
  # Détails
  if (stat) {
    nb_prod_ini <- length(data$productName)
    nb_levels_ini <- length(levels(factor(data$productName)))
  
    nb_levels_retire <- length(liste_prod_transfo_llm)
  
    nb_prod_fin <- length(data_bruts$productName)
    nb_levels_fin <- length(levels(factor(data_bruts$productName)))
    
    cat("Nombre de produits initial", data$nom_produit[1], ": ", nb_prod_ini, "levels: ", nb_levels_ini, "\n")
    cat("Nombre de produits retirés", data$nom_produit[1], ": ", nb_prod_ini - nb_prod_fin, "levels: ", nb_levels_retire, "\n")
    cat("Nombre de produits final", data$nom_produit[1], ": ", nb_prod_fin, "levels: ", nb_levels_fin, "\n")
    
  }
  
  return(data_bruts)
}


## --------------------------------------------------------------------------------
verifi_LLM_res <- lapply(liste_res_llm, function(x) lapply(x, extraire_liste_produits))

# Nombre de reponse null (c a d si l'extraction des levels dans les reponses du modèle de langage a échouée)
which(sapply(verifi_LLM_res, function(x) {all(sapply(x, is.null))}))


## --------------------------------------------------------------------------------
# Exécution de la fonction pour un seul prompt choisi ou pour plusieurs prompts
resultats_comparaison <- mapply(comparer_listes_produits, 
                                liste_prompts_produits_select, 
                                liste_res_llm, SIMPLIFY = FALSE)


## --------------------------------------------------------------------------------
### Voir la liste des vrais produits transformés trouver par le modèle

produit_transfo_llm <- (lapply(resultats_comparaison, function(x) {lapply(x, function(y) y$levels_communs)}))


## --------------------------------------------------------------------------------
table_indicateurs <- lapply(resultats_comparaison, function(x) { table_indicateurs_purete(x)})

#view(table_indicateurs)

## --------------------------------------------------------------------------------
# récupérer les données sélectionnées
data_tt <- liste_tables_37_produit_bruts[prod_select]

#Suppresion des levels de produits transformés
data_bruts <- mapply(
  FUN = retirer_prod_transfo,
  data_tt,
  resultats_comparaison[names(data_tt)],
  SIMPLIFY = FALSE
)


## --------------------------------------------------------------------------------
# Créer le dossier "data_bruts" s'il n'existe pas
dir.create("data_bruts", showWarnings = FALSE)

purrr::iwalk(data_bruts, function(df, nom) {
  nom_fichier <- paste0("data_", nom, "_bruts.xlsx")
  path_fichier <- file.path("data_bruts", paste0("data_", nom, "_bruts.xlsx"))
  write_xlsx(df, path_fichier)
  cat(nom_fichier, " sauvegarder! ", "\n")
  })

traceur <- function(colonne = NULL) {
  # ===== Fonctions traceur =====
  
  afficher_table_lignes <- function(lst, titre) {
    cat("\n", titre, "\n")
    lignes_par_table <- lapply(lst, nrow)
    invisible(mapply(function(nom, nb) {
      cat(nom, ": ", nb, "lignes\n")
    }, names(lignes_par_table), lignes_par_table))
    return(lignes_par_table)
  }
  
  afficher_distribution_par_colonne <- function(df, nom, colonne_sym) {
    df %>%
      group_by(plateforme, !!colonne_sym) %>%
      summarise(nb = n(), .groups = "drop") %>%
      group_by(plateforme) %>%
      mutate(`%` = round((nb / sum(nb)) * 100, 2)) %>%
      pivot_wider(names_from = !!colonne_sym, values_from = c(`%`, nb), values_fill = 0)
  }
  
  afficher_liste_tableaux <- function(lst, titre) {
    cat("\n", titre, "\n")
    invisible(mapply(function(nom, df) {
      cat("====", nom, "====\n")
      print(df)
      cat("\n")
    }, names(lst), lst))
  }
  
  # --- Étapes du traitement ---
  
  # 1. Données initiales
  d_init_annee <- afficher_table_lignes(liste_datasets_final, "Données initiales par plateforme par année")
  d_init_fusion <- nrow(combinaison)
  cat("\nDonnées fusionnées : ", d_init_fusion, "lignes\n")
  cat("Conservation :", round((d_init_fusion / sum(unlist(d_init_annee))) * 100, 2), "%\n")
  
  # 2. Expressions régulières
  cat("\nExpressions régulières\n")
  res_total <- nrow(resultats_2$data_ok) + nrow(resultats_2$data_pas_ok)
  cat("Total : ", res_total, "soit", round((res_total / d_init_fusion) * 100, 2), "% des données fusionnées\n")
  res_ok <- nrow(resultats_2$data_ok)
  cat("Regex OK : ", res_ok, "soit", round((res_ok / res_total) * 100, 2), "%\n")
  
  # 3. Retrait produits prix non calculables
  res_ok_final <- nrow(datasets_tts_plateformes_prod_tries)
  cat("\nRetrait produits prix non calculables : ", res_ok-res_ok_final, "retirés,", res_ok_final, "lignes restantes\n")
  cat("Conservation par rapport à regex OK : ", round((res_ok_final / res_ok) * 100, 2), "%\n")
  cat("Conservation par rapport à total regex : ", round((res_ok_final / res_total) * 100, 2), "%\n")
  
  # 4. Séparation en tables produits
  d_par_produit <- afficher_table_lignes(liste_tables_37_produit_bruts, "Séparation en 37 tables produits \n \n")
  cat("Conservation par rapport à regex OK final : ", round((sum(unlist(d_par_produit)) / res_ok_final) * 100, 2), "%\n")
  
  # 5. Traitement LLM
  cat("\nTraitement LLM\n")
  cat("Produits sélectionnés : ", paste(prod_select, collapse = ", "), "\n")
  
  nb_levels_par_produit <- setNames(
    lapply(prod_select, function(x) if (!is.null(liste_levels_37_produit[[x]])) nrow(liste_levels_37_produit[[x]]) else NA),
    prod_select
  )
  
  nb_ligne_par_produit <- setNames(
    lapply(prod_select, function(x) if (!is.null(liste_tables_37_produit_bruts[[x]])) nrow(liste_tables_37_produit_bruts[[x]]) else NA),
    prod_select
  )
  
  nb_levels_retire <- setNames(
    lapply(prod_select, function(x) {
      comp <- resultats_comparaison[[x]]
      if (is.null(comp)) return(NA)
      sum(sapply(comp, function(h) length(h$produit_transfo_llm)))
    }),
    prod_select
  )
  
  nb_ligne_restantes <- setNames(
    lapply(prod_select, function(x) if (!is.null(data_bruts[[x]])) nrow(data_bruts[[x]]) else NA),
    prod_select
  )
  
  invisible(mapply(function(nom, lv, lg, lv_r, lg_r) {
    if (is.na(lv) | is.na(lg) | is.na(lv_r) | is.na(lg_r)) {
      cat(nom, ": données manquantes\n")
    } else {
      cat(nom, ": initiales:", lv, "levels,", lg, "lignes | retirées:", lv_r, "levels,", 
          lg - lg_r, "lignes soit", round(((lg - lg_r) / lg) * 100, 2), "%\n")
    }
  }, prod_select, nb_levels_par_produit, nb_ligne_par_produit, nb_levels_retire, nb_ligne_restantes))
  
  
  # --- Si une colonne est fournie, suivre son évolution ---
  if (!is.null(colonne)) {
    colonne_sym <- rlang::sym(colonne)
    
    cat("\n\n=== Analyse de la variable :", colonne, "===\n")
    
    # Étapes d'agrégation
    a <- afficher_distribution_par_colonne(combinaison, "Données combinées", colonne_sym)
    b <- afficher_distribution_par_colonne(resultats_2$data_ok, "Regex OK", colonne_sym)
    c <- afficher_distribution_par_colonne(datasets_tts_plateformes_prod_tries, "Regex Final", colonne_sym)
    d <- lapply(liste_tables_37_produit_bruts, afficher_distribution_par_colonne, nom = "Séparation produits", colonne_sym)
    e <- lapply(data_bruts, afficher_distribution_par_colonne, nom = "LLM final", colonne_sym)
    
    calcul_tt_pourcentage <- function(df, ref_df) {
      cols <- grep("^nb_", names(df), value = TRUE)
      total_all <- Reduce(`+`, ref_df[cols])
      
      for (col in cols) {
        modality <- sub("^nb_", "", col)
        ref_col <- paste0("nb_", modality)
        
        if (!ref_col %in% names(ref_df)) next  # skip si modalité absente
        
        df[[paste0("tt_%_", modality)]] <- round((df[[col]] / total_all) * 100, 2)
      }
      return(df)
    }
    
    b <- calcul_tt_pourcentage(b, a)
    c <- calcul_tt_pourcentage(c, a)
    
    # Affichage
    cat("\nDonnées initiales combinées:\n"); print(a)
    cat("\nRegex OK:\n"); print(b)
    cat("\nRegex Final:\n"); print(c)
    
    afficher_liste_tableaux(d, "Séparation produits (37 tables)")
    afficher_liste_tableaux(e, "Données finales après LLM")
    
    e_names <- intersect(names(e), names(d))
    e <- mapply(function(df1, df2) {
      calcul_tt_pourcentage(df1, df2)
    }, e[e_names], d[e_names], SIMPLIFY = FALSE)
    
  }
  
  return(invisible(list(
    aggregation_combinaison = a,
    aggregation_regex_ok = b,
    aggregation_regex_final = c,
    aggregation_par_produit = d,
    aggregation_llm_final = e
  )))
  
}
a <- traceur("productIsOrganic")
b <- traceur("solde")
