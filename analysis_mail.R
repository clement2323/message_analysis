Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

source("fonctions.R")
packages <- c("jsonlite", "dplyr", "lubridate", "stringr", "tidyr", "ggplot2", "ggiraph")

sapply(packages, require, character.only = TRUE)

rm(list=ls())

source("fonctions.R")
# Lire le fichier JSON
data <- fromJSON("messages.json")
message_table <- data$messages

#message_table|>View()

indicatrice_previous_message <- sapply(message_table$previous_messages,function(message){
    return((message|> nrow() )>0 )
} )

message_table$date <- sapply(message_table$date,parse_custom_date)


# Assuming message_table$date has already been processed with parse_custom_date
message_table <- message_table %>%
  separate(date, into = c("annee", "mois", "jour", "heure", "minute", "seconde"), sep = "-", remove = FALSE)


# Extraire prénom et nom
message_table <- message_table %>%
  mutate(
    full_name = str_extract(sender, "^[^@]+"),
    full_name = str_replace_all(full_name, "\\.", " "),
    full_name = str_to_title(full_name)
  )

# Compter le nombre de mails par expéditeur, en excluant les NA
mail_counts <- message_table %>%
  filter(!is.na(full_name) & full_name != "") %>%
  count(full_name, sort = TRUE) %>%
  top_n(20, n)

# Créer le ggplot avec ggiraph
p <- ggplot(mail_counts, aes(x = reorder(full_name, n), y = n)) +
  geom_col_interactive(aes(tooltip = sprintf("%s: %d mails", full_name, n), data_id = full_name), fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Nombre de mails envoyés par les 20 expéditeurs les plus actifs",
    x = "Expéditeur",
    y = "Nombre de mails"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Convertir en graphique interactif
interactive_plot <- girafe(ggobj = p, width_svg = 10, height_svg = 8)

# Personnaliser l'apparence au survol
interactive_plot <- girafe_options(interactive_plot,
  opts_hover(css = "fill:orange;"),
  opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;")
)

# Afficher le graphique interactif
interactive_plot

# Sauvegarder le graphique interactif en HTML
htmlwidgets::saveWidget(interactive_plot, "top_20_senders_interactive.html")


# - dico structuré, save en §JSON pour lecture sur R après
# - nombre de mail par jour
# - tableau de bord des heures du nombre envois de mail par personne
# - graph entre les individus qui répondent etc..et se renvoit
# - nombre de mots par mail, nombre de mots uniques utilisés etc..
# - liens pondérés par nb mails envoyés, couleurs des noeuds dépendant de la div
# - faire des records
# - mail le plus long
 #C:/Users/RK09OA/AppData/Local/Programs/Python/Python312/python.exe , pour lancer python sur terminal
# retravailler sur la structure des mails

