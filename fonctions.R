
# Convertir les dates en format POSIXct et ajuster pour le fuseau horaire de la Martinique
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

installer_package <-  function(pkg){
  print(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}