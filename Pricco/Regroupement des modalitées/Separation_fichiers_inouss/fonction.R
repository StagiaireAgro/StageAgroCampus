
res <- split_data(data_socleo_2021, fruits)
res$clean_name <- gsub("\\s+", "", tolower(res$clean_name))


# Fonction pour transformer un mot-clé en regex souple
transformer_en_regex <- function(mot) {
  mots <- str_split(mot, " ")[[1]]
  mots_regex <- sapply(mots, function(m) paste0(m, "(s)?"))
  paste0(mots_regex, collapse = "")
}

# Préparation des mots-clés : regex souples
mots_regex <- sapply(fruits, transformer_en_regex)
mots_sans_espaces <- gsub(" ", "", tolower(fruits))

# Trier du plus long au plus court
ordre <- order(nchar(mots_sans_espaces), decreasing = TRUE)
mots_regex <- mots_regex[ordre]
fruits_triee <- fruits[ordre]
mots_sans_espaces <- mots_sans_espaces[ordre]

# Détection sans chevauchement
res$classif <- sapply(res$clean_name, function(texte) {
  detectes <- c()
  texte_tmp <- texte
  
  for (i in seq_along(mots_regex)) {
    pattern <- mots_regex[i]
    if (str_detect(texte_tmp, regex(pattern, ignore_case = TRUE))) {
      detectes <- c(detectes, fruits_triee[i])
      # Supprimer la portion détectée du texte temporaire
      texte_tmp <- str_replace(texte_tmp, regex(pattern, ignore_case = TRUE), "")
    }
  }
  paste(unique(detectes), collapse = ", ")
})

liste_par_fruit <- list()

# Ajouter les cas où un seul fruit est détecté
for (fruit in fruits) {
  liste_par_fruit[[fruit]] <- res[res$classif == fruit, ]
}



ls(liste_par_fruit)
View(liste_par_fruit$`pomme de terre`)

# Ajouter les cas multi-fruits dans "autre"
liste_par_fruit[["autre"]] <- res[str_count(res$classif, ",") >= 1, ]
