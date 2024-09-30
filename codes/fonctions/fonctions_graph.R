# Fonction pour créer un graphe de réseau des expéditeurs et destinataires
plot_email_network <- function(message_table, non_personne_emails, n_recipients = 0, n_edges = 10) {
  # n_recipients = 0; n_edges = 20
  # Compter les occurrences de chaque destinataire
  recipient_counts <- table(unlist(message_table$recipients))
  # Filtrer les destinataires qui apparaissent plus de n_recipients fois
  frequent_recipients <- names(recipient_counts[recipient_counts > n_recipients])
  
  # Créer un vecteur unique d'expéditeurs et destinataires fréquents
  frequent_recipients <- setdiff(frequent_recipients, non_personne_emails)%>%unique()
  frequent_recipients <- setdiff(frequent_recipients, "clement.guillo@insee.fr")
 
  # Créer les nœuds
  nodes <- data.frame(
    id = frequent_recipients,
    label = frequent_recipients,
    color = "red",
    size = 15
  ) 
  # Créer les arêtes
  edges <- do.call(rbind, lapply(1:nrow(message_table), function(i) {
    #i <-50
    sender <- message_table$sender[i]

    recipients <- setdiff(message_table$recipients[[i]], non_personne_emails)
    recipients <- setdiff(recipients, "clement.guillo@insee.fr")
    recipients <- recipients[recipients %in% frequent_recipients]

    if(length(recipients) == 0 | !(sender %in% frequent_recipients) ) return(data.frame())

    data.frame(
      from = rep(sender, length(recipients)),
      to = recipients
    )
  }))

  # Grouper les arêtes et ne garder que celles qui apparaissent plus de n_edges fois
  edges <- edges %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop") %>%
    filter(weight > n_edges)

  # Ajouter la colonne arrows
  edges$arrows <- "to"
  
  # Filtrer les nœuds qui apparaissent dans les arêtes
  nodes_in_edges <- unique(c(edges$from, edges$to))
  nodes <- nodes %>%
    filter(id %in% nodes_in_edges)

  id_to_full_name <- sapply(nodes$id,extract_full_name)
  nodes$id  <- id_to_full_name[nodes$id]
  nodes$label <- nodes$id
  
  #edges <- edges%>%
   # filter(from %in% names(id_to_full_name), to %in% names(id_to_full_name))
  #which(!edges$from %in% names(id_to_full_name))
  edges$from <- id_to_full_name[edges$from]
  edges$to <- id_to_full_name[edges$to]
  
  
  # Créer le graphique interactif avec visNetwork
  visNetwork(nodes, edges) %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}

# # Utiliser la fonction avec n_recipients = 200 et n_edges = 10 par exemple
# email_network_lien_5 <- plot_email_network(message_table, non_personne_emails, n_recipients = 0, n_edges = 20)
# email_network_lien_5 <- plot_email_network(message_table, non_personne_emails, n_recipients = 0, n_edges = 5)
# email_network_lien_10 <- plot_email_network(message_table, non_personne_emails, n_recipients = 0, n_edges = 10)
# email_network_lien_5 <- plot_email_network(message_table, non_personne_emails, n_recipients = 200, n_edges = 5)
# email_network_lien_7 <- plot_email_network(message_table, non_personne_emails, n_recipients = 10, n_edges = 7)
# # Sauvegarder le graphe interactif
# htmlwidgets::saveWidget(email_network_lien_5, "output/email_network_lien_5.html", selfcontained = FALSE)
# htmlwidgets::saveWidget(email_network_lien_10, "output/email_network_lien_10.html", selfcontained = FALSE)
# htmlwidgets::saveWidget(email_network_lien_7, "output/email_network_lien_7.html", selfcontained = FALSE)


