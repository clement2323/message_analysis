#' Install a package if it's not already installed, using Lyon 1 CRAN mirror
#'
#' This function checks if a package is installed and installs it if it's not,
#' using the Lyon 1 CRAN mirror.
#'
#' @param pkg A character string specifying the name of the package to install.
#' @return None
#' @export
installer_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}



nettoyer_html <- function(texte) {
  texte_nettoye <- gsub("<.*?>", "", texte)
  texte_nettoye <- gsub("\\s+", " ", texte_nettoye)
  texte_nettoye <- trimws(texte_nettoye)
  return(texte_nettoye)
}

#' Parse custom date format and adjust for Martinique timezone
#'
#' This function parses a custom date string and adjusts it to the Martinique timezone (UTC-4).
#'
#' @param date_string A character string representing a date in the format "Day, DD Mon YYYY HH:MM:SS +/-HHMM".
#' @return A character string representing the parsed and adjusted date-time.
#' @export
parse_custom_date <- function(date_string) {
    #date_string <- "Wed, 6 Mar 2024 08:29:01 -0400"

    if (is.na(date_string)) return(NA)
    
    pattern <- "([A-Za-z]{3}),\\s(\\d{1,2})\\s([A-Za-z]{3})\\s(\\d{4})\\s(\\d{2}):(\\d{2}):(\\d{2})\\s([+-]\\d{4})"
    matches <- str_match(date_string, pattern)
    
    if (is.na(matches[1])) return(NA)
    
    day_of_week <- matches[2]
    day <- as.integer(matches[3])
    month <- match(matches[4], month.abb)
    year <- as.integer(matches[5])
    hour <- as.integer(matches[6])
    offset <- matches[9]
    
    # Ajuster l'heure manuellement en fonction de l'offset
    offset_hours <- as.integer(substr(offset, 2, 3))
    
    # Convertir en heure des Antilles (UTC-4)
    antilles_offset <- -4
    hour_diff <- antilles_offset - (if(substr(offset, 1, 1) == "+") 1 else -1) * offset_hours
    
    adjusted_hour <- (hour + hour_diff) %% 24
    
    # Construire la chaîne de date
    date_time <- paste(year, sprintf("%02d", month), sprintf("%02d", day), 
                       sprintf("%02d", adjusted_hour), matches[7], matches[8], sep = "-")
    
    date_time
}

# Fonction pour extraire et formater le nom complet
extract_full_name <- function(sender) {
  full_name <- str_extract(sender, "^[^@]+")
  full_name <- str_replace_all(full_name, "\\.", " ")
  full_name <- str_to_title(full_name)
  return(full_name)
}


nettoyer_sujet <- function(sujet) {
    #sujet <- messages_filtres$subject[29]
    # Remplacer =?iso-8859-1?Q? par un espace
    sujet_modifie <- gsub("=\\?iso-8859-1\\?Q\\?", " ", sujet)
    sujet_modifie <- gsub("_", " ", sujet_modifie)
    sujet_modifie <- gsub("=E9", "é", sujet_modifie)
    sujet_modifie <- gsub("C3A9", "é", sujet_modifie)
    sujet_modifie <- gsub("C3A0", "à", sujet_modifie)
    # Supprimer les encodages restants et caractères spéciaux
    sujet_propre <- gsub("[?=]", "", sujet_modifie)
    sujet_propre <- gsub("\\r\\n", " ", sujet_propre)
    sujet_propre <- gsub("\\s+", " ", sujet_propre)
    sujet_propre <- trimws(sujet_propre)

    return(sujet_propre)
}


add_regex_stopwords <- function(stopwords, pattern, data, description) {
  # Trouver les mots correspondant au pattern
  matching_words <- data %>%
    filter(str_detect(word, pattern)) %>%
    count(word, sort = TRUE) %>%
    pull(word)
  
  # Ajouter ces mots à la liste des stopwords
  new_stopwords <- unique(c(stopwords, matching_words))
  
  # Afficher le nombre de nouveaux mots ajoutés
  new_words_count <- length(new_stopwords) - length(stopwords)
  message(paste("Ajout de", new_words_count, "nouveaux stopwords pour", description))
  
  return(new_stopwords)
}

nettoyer_corps_message <- function(message_content) {   
   # message_content <- messages_filtres$body[14]
  cleaned_body <- message_content %>%
    str_replace_all("\\b(\\+?\\d{1,4}[-.\\s]?)?\\(?\\d{1,4}\\)?[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,9}\\b", "") %>%
    str_replace_all("\\b[\\w\\s]+\\s+<[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}(\\s*<mailto:[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}>)?\\s*>", "") %>%
    str_replace_all("\\b[A-Z][a-z]+\\s+[A-Z][a-z]+[-]?;", "") %>%
    str_replace_all(";\\s*$", "") %>%
    str_replace_all(";\\s*\n", "\n") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    gsub(pattern = ";", replacement = "")
  
  parts <- str_split(cleaned_body, "(?i)\\bDe\\s*:")[[1]]
  parts <- lapply(parts, str_trim)
  parts <- parts[parts != ""]
  
  cleaned_body <- unlist(parts)
  cleaned_body <- paste(cleaned_body, collapse = "\n ************** \n")
  return(cleaned_body)
}