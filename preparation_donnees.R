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

# Extraire prénom, nom et détecter l'origine du mail
message_table <- message_table %>%
  mutate(
    full_name = str_extract(sender, "^[^@]+"),
    full_name = str_replace_all(full_name, "\\.", " "),
    full_name = str_to_title(full_name)
  )

message_table$date <- sapply(message_table$date,parse_custom_date)

message_table <- message_table %>%
  separate(date, into = c("annee", "mois", "jour", "heure", "minute", "seconde"), sep = "-", remove = FALSE)

# Ajouter la variable nb_recipients
message_table <- message_table %>%
  mutate(nb_recipients = sapply(recipients, length))

# Filtrer les messages (si nécessaire)
message_table <- message_table %>% 
  filter(!is.na(date), !is.na(full_name))
