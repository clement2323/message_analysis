installer_package <-  function(pkg){
  print(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

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


# Créer une fonction pour générer le graphique pour une période donnée
create_nb_mail_plot <- function(data, months_back,n_user=40) {
  # data = message_table ; months_back = 10
  date_filter <- Sys.Date() - months(months_back)
  
  mail_counts <- data %>%
    filter(!is.na(full_name) & full_name != "") %>%
    filter(as.Date(paste(annee, mois, jour, sep = "-")) >= date_filter)
  # Extraire les dates les plus anciennes et les plus récentes

  date_range <- mail_counts %>%
    summarise(
      oldest_date = min(as.Date(paste(annee, mois, jour, sep = "-")), na.rm = TRUE),
      newest_date = max(as.Date(paste(annee, mois, jour, sep = "-")), na.rm = TRUE)
    )
    
  mail_counts <- mail_counts %>%
    count(full_name, sort = TRUE) %>%
    top_n(n_user, n)
  
  p <- ggplot(mail_counts, aes(x = reorder(full_name, n), y = n)) +
    geom_col_interactive(aes(tooltip = sprintf("%s: %d mails", full_name, n), 
                             data_id = full_name, 
                             onclick = sprintf("Shiny.setInputValue(`selected_sender`, `%s`)", gsub("'", "\\\\'", full_name))), 
                         fill = "red") +
    coord_flip() +
    labs(
      title = sprintf("Nombre de mails envoyés par les %d expéditeurs les plus actifs", n_user),
      x = "Expéditeur",
      y = "Nombre de mails",
      caption = sprintf("Période : du %s au %s", 
                        format(date_range$oldest_date, "%d/%m/%Y"),
                        format(date_range$newest_date, "%d/%m/%Y"))
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  
  girafe(ggobj = p, width_svg = 10, height_svg = 8) %>%
  girafe_options(opts_hover(css = "fill:orange;"),
                 opts_selection(type = "single", css = "fill:navy;"))
}


create_hourly_plot<- function(message_table,months_back) {
  # Filtrer les données pour les derniers mois
  months_ago <- Sys.Date() - months(months_back)
  last_months <- message_table %>%
    filter(date >= months_ago)
  
  # Créer un résumé des données par heure
  hourly_summary <- last_months %>%
    group_by(heure) %>%
    summarise(count = n()) %>%
    mutate(heure = as.factor(heure))
  
  # Créer le graphique
  hourly_plot <- ggplot(hourly_summary, aes(x = heure, y = count)) +
    geom_bar_interactive(
      stat = "identity",
      aes(tooltip = paste("Heure:", heure, "\nNombre de mails:", count),
          data_id = heure),
      fill = "skyblue"
    ) +
    labs(title = sprintf("Répartition des mails par heure (%d derniers mois)",months_back),
         x = "Heure", y = "Nombre de mails") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Générer le graphique interactif
  interactive_hourly_plot <- girafe(ggobj = hourly_plot)
  
  return(interactive_hourly_plot)
}

# Utilisation de la fonction