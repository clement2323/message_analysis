# n_mail par sender 
interactive_plot <- create_nb_mail_plot(message_table,5,20)
htmlwidgets::saveWidget(interactive_plot, "output/top_senders_interactive.html",selfcontained=FALSE)

message_table%>% filter(sender_type =="personne")%>% pull(sender) %>%unique()

# n_mail ar heure
hourly_plot <- create_hourly_plot(message_table %>% filter(sender_type =="personne"),12)

# Animation sender race
animated_plot <- create_top_senders_animation(message_table %>% filter(sender_type =="personne"), top_n = 15, months_back = 6,1400,10)
anim_save("output/top_senders_animated.gif", animated_plot)

# n mail dans heure x semaine
heatmap_plot <- create_heatmap(message_table)
ggsave("output/heatmap_mails_jour_heure.png", heatmap_plot, width = 12, height = 8)


# nb unique sender
unique(message_table$sender)%>%length()

# mail en provenance d'un vrai mail
message_table%>%filter(sender_type == "personne")%>%nrow()

# mail directement à moi en provenance de quequ'un
message_table%>%filter(sender_type == "personne" & nb_recipients ==1 )%>%nrow()

interactive_plot <- create_nb_mail_plot(
  message_table%>%filter(sender_type == "personne" ),12,20)

# seulement à moi
interactive_plot <- create_nb_mail_plot(
  message_table%>%filter(sender_type == "personne" & nb_recipients == 1 ),12,20)

# Save the interactive plot
interactive_stacked_bar <- create_hourly_plot_detailed(message_table)
htmlwidgets::saveWidget(interactive_stacked_bar, "output/affinage_nb_mail.html", selfcontained = TRUE)

# save dans giff etc..
create_top_senders_animation(message_table%>% filter(sender_type == "personne"))
(message_table%>%filter(nb_recipients>1)%>%pull(recipients))[1]


#
email_network_lien_7 <- plot_email_network(message_table, non_personne_emails, n_recipients = 10, n_edges = 7)
htmlwidgets::saveWidget(email_network_lien_7, "output/email_network_lien_7_bis.html", selfcontained = FALSE)

