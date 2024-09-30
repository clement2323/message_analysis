#' Install a package if it's not already installed
#'
#' This function checks if a package is installed and installs it if it's not.
#'
#' @param pkg A character string specifying the name of the package to install.
#' @return None
#' @export
installer_package <- function(pkg) {
  print(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
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


