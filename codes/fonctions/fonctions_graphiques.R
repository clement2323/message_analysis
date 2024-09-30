#' Create a plot of the number of emails sent by top senders
#'
#' This function generates an interactive bar plot showing the number of emails sent by the most active senders.
#'
#' @param data A data frame containing email data.
#' @param months_back An integer specifying how many months back to consider.
#' @param n_user An integer specifying the number of top senders to display (default is 40).
#' @return A girafe object containing the interactive plot.
#' @export
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

#' Create an hourly distribution plot of emails
#'
#' This function generates an interactive bar plot showing the distribution of emails by hour.
#'
#' @param message_table A data frame containing email data.
#' @param months_back An integer specifying how many months back to consider.
#' @return A girafe object containing the interactive hourly plot.
#' @export
create_hourly_plot <- function(message_table, months_back, subtitle = NULL) {
  # Filtrer les données pour les derniers mois
  months_ago <- Sys.Date() - months(months_back)
  last_months <- message_table %>%
    filter(date >= months_ago)
  
  # Créer un résumé des données par heure
  hourly_summary <- last_months %>%
    group_by(heure) %>%
    summarise(count = n()) %>%
    mutate(heure = as.factor(heure))
  
  # Modifier le titre pour inclure le sous-titre s'il est fourni
  title <- sprintf("Répartition des mails par heure (%d derniers mois)", months_back)
  if (!is.null(subtitle)) {
    title <- paste0(title, "\n(", subtitle, ")")
  }

  # Créer le graphique
  hourly_plot <- ggplot(hourly_summary, aes(x = heure, y = count)) +
    geom_bar_interactive(
      stat = "identity",
      aes(tooltip = paste("Heure:", heure, "\nNombre de mails:", count),
          data_id = heure),
      fill = "skyblue"
    ) +
    labs(title = title,
         x = "Heure", y = "Nombre de mails") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Générer le graphique interactif
  interactive_hourly_plot <- girafe(ggobj = hourly_plot)
  
  return(interactive_hourly_plot)
}

# Utilisation de la fonction


#' Extract recent emails and save to CSV
#'
#' This function extracts emails from the last two weeks and writes their content to a CSV file.
#'
#' @param message_table A data frame containing email data.
#' @param output_file A character string specifying the path and filename for the output CSV file.
#' @return None
#' @export
extract_recent_emails <- function(message_table, output_file) {
  # Convertir la colonne 'date' en format Date si ce n'est pas déjà fait
  message_table$date <- as.Date(message_table$date)
  
  # Calculer la date d'il y a deux semaines
  two_weeks_ago <- Sys.Date() - 14
  
  # Filtrer les messages des deux dernières semaines
  recent_messages <- message_table %>%
    filter(date >= two_weeks_ago) %>%
    select(date, sender, subject, body)
  
  # Écrire les données dans un fichier CSV
  write.csv(recent_messages, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("Les e-mails récents ont été extraits et enregistrés dans", output_file, "\n")
}


#' Create an animated plot of top email senders
#'
#' This function generates an animated bar plot showing the evolution of top email senders over time.
#'
#' @param data A data frame containing email data.
#' @param top_n An integer specifying the number of top senders to display (default is 15).
#' @param months_back An integer specifying how many months back to consider (default is 6).
#' @return None (saves the animation as a GIF file)
#' @export
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
  p <- ggplot(animation_data, aes(x = ranking, y = cumulative_n, group = full_name, 
                                  fill = as.factor(ranking %% 2))) +  # Alternance des couleurs
    geom_col() +
    geom_text(aes(label = full_name), hjust = -0.1, color = "black", size = 3) +
    #geom_text(aes(label = cumulative_n), hjust = -0.1) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_x_reverse() +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("lightblue", "lightgrey")) +  # Définir les couleurs
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
  animated_plot <- animate(p, nframes = 400, fps = 10, width = 800, height = 600, renderer = gifski_renderer())
  anim_save("output/top_senders_animated.gif", animated_plot)
}

#' Create a heatmap of email activity
#'
#' This function generates a heatmap showing email activity by day of the week and hour of the day.
#'
#' @param email_data A data frame containing email data with 'date' and 'heure' columns.
#' @return A ggplot object representing the heatmap.
#' @export
create_heatmap <- function(data, subtitle = NULL) {
  data <- data %>%
    mutate(
      weekday = weekdays(as.Date(date)),
      hour = as.numeric(heure)
    )
  
  heatmap_data <- data %>%
    count(weekday, hour) %>%
    complete(weekday, hour = 0:23, fill = list(n = 0))
  
  heatmap_data$weekday <- factor(heatmap_data$weekday, 
                                 levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
  
  # Construire le titre
  title <- "La danse des e-mails : Quand vos messages font la fête !"
  if (!is.null(subtitle)) {
    title <- paste0(title, "\n(", subtitle, ")")
  }
  
  ggplot(heatmap_data, aes(x = hour, y = weekday, fill = n)) +
    geom_tile_interactive(aes(tooltip = sprintf("Jour: %s\nHeure: %d\nNombre de mails: %d", weekday, hour, n))) +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_continuous(breaks = 0:23) +
    labs(title = title,
         x = "Heure de la journée", 
         y = "Jour de la semaine", 
         fill = "Nombre de mails") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))  # Centre le titre
}

creer_nuage_mots <- function(tidy_bodies_clean,mail_sender = "all"){

    tidy_personne <- 
    if(mail_sender !="all") tidy_bodies_clean%>% filter(sender==mail_sender) else tidy_bodies_clean

    word_counts <- tidy_personne %>%
    count(word, sort = TRUE)

    wordcloud_graph <- wordcloud(words = word_counts$word, 
            freq = word_counts$n, 
            min.freq = 2,
            max.words = 100, 
            random.order = FALSE, 
            rot.per = 0.35, 
            colors = brewer.pal(8, "Dark2"))

    # Sauvegarder le wordcloud dans un fichier
    png(paste0("wordcloud_",mail_sender,".png"), width = 800, height = 600)
    wordcloud_graph
    dev.off()

}
