source("codes/setup.R")
source("codes/fonctions.R")
source("codes/preparation_donnees.R")

# n_mail par sender
interactive_plot <- create_nb_mail_plot(message_table,5,20)
htmlwidgets::saveWidget(interactive_plot, "output/top_senders_interactive.html",selfcontained=FALSE)

# n_mail ar heure
hourly_plot <- create_hourly_plot_last_6_months(message_table,12)
create_top_senders_animation(message_table, top_n = 15, months_back = 6)

# n mail dans heure x semaine
heatmap_plot <- create_heatmap(message_table %>% filter(from_dom))
ggsave("output/heatmap_mails_jour_heure.png", heatmap_plot, width = 12, height = 8)

### pour le champ des mails, il s'agit des mails que j'ai reçu
## certains viennnet de métropole 

# Create histogram of previous messages
previous_messages_count <- message_table %>%
  filter(indicatrice_previous_message) %>%
  mutate(prev_msg_count = sapply(previous_messages, nrow)) %>%
  pull(prev_msg_count)

histogram_plot <- ggplot(data.frame(count = previous_messages_count), aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Previous Messages Count",
       x = "Number of Previous Messages",
       y = "Frequency") +
  theme_minimal()

ggsave("output/previous_messages_histogram.png", histogram_plot, width = 10, height = 6)

# Filter out recipients with count of 1 and create histogram
table(message_table$nb_recipients)
message_table%>% filter(nb_recipients==0)%>% head(2)

### TO DO :
- regardermieux ls données sources oour voir si on put pas avoir la timezone quelque part dol etc...
- gérer dans les données sources le multimail pour récupérer l'information avec l'idée du mail enfant / parent  -> idée de cascade
- faire le graphe des destinataires
- créer un semainier par utilisateurs représentatif plus de X mail et faire un algo EM lena carell  classification -> semainier moyen (air bnb)
- créer une ACP parutilisateurs en créant des indicateurs type :
  - nb mots moyen
  - nb mots uniques utilisés
  - longueur mail moyenne
  - nb_mail_apres Xh 
  - nb_mail apres Xh
  - nb destinataires moyen
  - nb _connexions (destinataires global)
- gérer les champs : mails globauux, mails reçus de la métropole, mails reçus que des DOMS, hors boite fonctionnelle
- faire de lanalyse de sentiment
- restructurer avec chat GPT
- appli WEB
- dashboard 
- appeler Jeremyu pour éventuelle collaboration -> une ofis première version bien propre, tout type de client mail etc.., API outlook, proposer ça à des boites
