# Lire le fichier JSON
data <- fromJSON("input/messages.json")
message_table <- data$messages

message_table$parent_id <- NULL
message_table$indicatrice_previous_message <- sapply(message_table$previous_messages,function(message){
    return((message|> nrow() )>0 )
} )

# Fonction pour extraire le fuseau horaire
extract_timezone <- function(date_string) {
  tz <- str_extract(date_string, "[+-]\\d{4}$")
  return(ifelse(is.na(tz), "Unknown", tz))
}

# Appliquer la fonction pour extraire prénom et nom
message_table <- message_table %>%
  mutate(full_name = sapply(sender, extract_full_name))

message_table$date <- sapply(message_table$date,parse_custom_date)

message_table <- message_table %>%
  separate(date, into = c("annee", "mois", "jour", "heure", "minute", "seconde"), sep = "-", remove = FALSE)

# Ajouter la variable nb_recipients
message_table <- message_table %>%
  mutate(nb_recipients = sapply(recipients, length))

# Create histogram of previous messages
message_table <- message_table %>%
  mutate(prev_msg_count = sapply(previous_messages, nrow)) 

# fair une colonne de recipients sans boite fonctionnelle
# Créer la nouvelle variable
message_table <- message_table %>%
  mutate(sender_type = ifelse(sender%>%tolower() %in% non_personne_emails, "non_personne", "personne"))

# Filtrer les messages (si nécessaire)
message_table <- message_table %>% 
  filter(!is.na(date), !is.na(full_name))


# Fonction pour extraire le prénom et le nom
extract_name <- function(full_name) {
  parts <- strsplit(full_name, " ")[[1]]
  if (length(parts) >= 2) {
    return(list(prenom = parts[1], nom = paste(parts[-1], collapse = " ")))
  } else {
    return(list(prenom = full_name, nom = NA))
  }
}



