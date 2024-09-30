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
heatmap_plot <- create_heatmap(message_table)
ggsave("output/heatmap_mails_jour_heure.png", heatmap_plot, width = 12, height = 8)

### pour le champ des mails, il s'agit des mails que j'ai reçu
## certains viennnent de métropole 


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

# nb unique sender
unique(message_table$sender)%>%length()

# mail en provenance d'un vrai mail
message_table%>%filter(sender_type == "personne")%>%nrow()

# mail directement à moi en provenance de quequ'un
message_table%>%filter(sender_type == "personne" & nb_recipients ==1 )%>%nrow()

interactive_plot <- create_nb_mail_plot(
  message_table%>%filter(sender_type == "personne" ),12,20)

interactive_plot <- create_nb_mail_plot(
  message_table%>%filter(sender_type == "personne" & nb_recipients ==1 ),12,20)

# Décomosition des mails envoyés par opersone, à moi seul ou à plusieurs
# Prepare data for the stacked bar histogram
sender_summary <- message_table %>%
  filter(sender_type == "personne") %>%
  group_by(full_name) %>%
  summarise(
    multiple_recipients = sum(nb_recipients > 1),
    single_recipient = sum(nb_recipients == 1),
    total = n()
  ) %>%
  arrange(desc(total)) %>%
  head(20)  # Limit to top 20 senders

# Create the stacked bar histogram using ggplot2 and ggiraph
stacked_bar_plot <- ggplot(sender_summary, aes(y = reorder(full_name, total))) +
  geom_bar_interactive(
    aes(x = multiple_recipients, fill = "Multiple Recipients",
        tooltip = sprintf("Sender: %s\nMultiple Recipients: %d", full_name, multiple_recipients)),
    stat = "identity"
  ) +
  geom_bar_interactive(
    aes(x = single_recipient, fill = "Single Recipient",
        tooltip = sprintf("Sender: %s\nSingle Recipient: %d", full_name, single_recipient)),
    stat = "identity"
  ) +
  scale_fill_manual(values = c("Multiple Recipients" = "darkblue", "Single Recipient" = "lightblue")) +
  labs(title = "Number of Emails Received by Sender",
       y = "Sender",
       x = "Number of Emails",
       fill = "Recipient Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))

# Convert to an interactive plot
interactive_stacked_bar <- girafe(ggobj = stacked_bar_plot)

# Save the interactive plot
htmlwidgets::saveWidget(interactive_stacked_bar, "output/stacked_bar_senders_interactive.html", selfcontained = TRUE)

# save dans giff etc..
create_top_senders_animation(message_table%>% filter(sender_type == "personne"))

(message_table%>%filter(nb_recipients>1)%>%pull(recipients))[1]

### TO DO :
- Réaffiner un peu la fonction graph, pondérer liens etc.., sélection de mail possible si on veut un graph récis
- regardermieux ls données sources oour voir si on put pas avoir la timezone quelque part dol etc...
- gérer dans les données sources le multimail pour récupérer l'information avec l'idée du mail enfant / parent  -> idée de cascade
- faire le graphe des destinataires
- créer un semainier par utilisateurs représentatif plus de X mail et faire un algo EM lena carell  classification -> semainier moyen (air bnb)
- créer une ACP par utilisateurs en créant des indicateurs type :
  - nb mots moyen
  - nb mots uniques utilisés
  - longueur mail moyenne
  - nb_mail_apres Xh 
  - nb_mail apres Xh
  - nb destinataires moyen
  - nb connexions (destinataires global)
- gérer les champs : mails globauux, mails reçus de la métropole, mails reçus que des DOMS, hors boite fonctionnelle
- faire de lanalyse de sentiment
- restructurer avec chat GPT
-embeddings personnes et tsne

library(ollamar)etllamaen self
embeddings word to vec, words cloud avec sentiment analysis,
tidy text
- chaines de réponses sous forme de graph 
-calculs nouedsinfluents et coloration
- recuperation indicatrice DOM ou nnon evntuellemnt avec listing
- acm avec n_destinatiaresdifférents
- heat map avec facet wrap
- appli WEB
- dashboard
- appeler Jeremyu pour éventuelle collaboration -> une ofis première version bien propre, tout type de client mail etc.., API outlook, proposer ça à des boites
- analyse depctage de haterspar personne
- un nuage de mots par ersonne (pour les plus gros expediteurs)
- neo4j base graphe, faire untruc plus en mode base de données 3FN au leiu de tout trimballer
- nb messages reçu par jour mois etc.. cumulatif avec date de début
- regarder le nb de messages par mois aussi ça notera les vaces etc.. et le faire par semaine pour un agent pour connaitre ses vacances
- commencer à conaténer tout ça