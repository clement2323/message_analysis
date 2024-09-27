Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

rm(list=ls())

packages <- c("jsonlite", "dplyr", "lubridate", "stringr", "tidyr", "ggplot2", "ggiraph","gganimate")
sapply(packages, require, character.only = TRUE)

source("codes/fonctions.R")

# Lire le fichier JSON
data <- fromJSON("input/messages.json")
message_table <- data$messages

indicatrice_previous_message <- sapply(message_table$previous_messages,function(message){
    return((message|> nrow() )>0 )
} )
message_table$date <- sapply(message_table$date,parse_custom_date)

message_table <- message_table %>%
  separate(date, into = c("annee", "mois", "jour", "heure", "minute", "seconde"), sep = "-", remove = FALSE)

# Extraire prénom et nom
message_table <- message_table %>%
  mutate(
    full_name = str_extract(sender, "^[^@]+"),
    full_name = str_replace_all(full_name, "\\.", " "),
    full_name = str_to_title(full_name)
  )

interactive_plot <- create_nb_mail_plot(message_table,5,20)
# Sauvegarder le graphique interactif en HTML
htmlwidgets::saveWidget(interactive_plot, "output/top_senders_interactive.html",selfcontained=FALSE)

## heure repartition
# Assurez-vous que la colonne 'heure' est de type numérique
message_table$heure <- as.numeric(message_table$heure)

# Créer un dataframe résumé pour l'histogramme
mail_count <- message_table %>%
  count(heure)
# compte par personne 
interactive_plot <- create_plot(message_table,6)
saveWidget(interactive_plot, "repartition_mails_par_heure_interactif.html")

# créer heure par heure avec le decoupage que j'avais fait déja bar plot ggiraph aussi 6 derniers mois fonction

hourly_plot <- create_hourly_plot_last_6_months(message_table,12)

# ... existing code ...

# Créer l'animation des top expéditeurs
create_top_senders_animation <- function(data, top_n = 15, months_back = 6) {
  #data <-message_table
  # Assurez-vous que la colonne 'date' est au format Date
  data$date <- as.Date(data$date)
  
  # Filtrer les données pour les derniers mois
  end_date <- max(data$date,na.rm = TRUE)
  start_date <- end_date - months(months_back)
  filtered_data <- data %>% 
    filter(date >= start_date)
  
  # Préparer les données pour l'animation
  animation_data <- filtered_data %>%
    count(full_name, annee, mois) %>%
    arrange(full_name, annee, mois) %>%
    group_by(full_name) %>%
    mutate(
      cumulative_n = cumsum(n),
      date = as.Date(paste(annee, sprintf("%02d", as.numeric(mois)), "01", sep = "-"))
    ) %>%
    group_by(annee, mois) %>%
    mutate(ranking = row_number(-cumulative_n)) %>%
    filter(ranking <= top_n) %>%
    ungroup()

  # Créer l'animation
  p <- ggplot(animation_data, aes(x = ranking, y = cumulative_n, group = full_name, fill = full_name)) +
    geom_col() +
    geom_text(aes(label = full_name), hjust = 1.1, color = "white") +
    #geom_text(aes(label = cumulative_n), hjust = -0.1) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_x_reverse() +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = margin(1, 4, 1, 3, "cm")
    ) +
    labs(title = 'Top {top_n} expéditeurs de mails', 
         subtitle = 'Date: {frame_time}',
         x = "", y = "Nombre de mails") +
    transition_time(date) +
    ease_aes('linear')

  # Animer et sauvegarder
  animated_plot <- animate(p, nframes = 100, fps = 5, width = 800, height = 600, renderer = gifski_renderer())
  anim_save("output/top_senders_animated.gif", animated_plot)
}

# Appeler la fonction pour créer l'animation
create_top_senders_animation(message_table, top_n = 15, months_back = 6)
