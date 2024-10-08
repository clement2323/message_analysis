nettoyer_corps_message <- function(message_content) {   
   # message_content <- messages_filtres$body[14]
  cleaned_body <- message_content %>%
    str_replace_all("\\b(\\+?\\d{1,4}[-.\\s]?)?\\(?\\d{1,4}\\)?[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,9}\\b", "") %>%
    str_replace_all("\\b[\\w\\s]+\\s+<[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}(\\s*<mailto:[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}>)?\\s*>", "") %>%
    str_replace_all("\\b[A-Z][a-z]+\\s+[A-Z][a-z]+[-]?;", "") %>%
    str_replace_all(";\\s*$", "") %>%
    str_replace_all(";\\s*\n", "\n") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    gsub(pattern = ";", replacement = "")
  
  parts <- str_split(cleaned_body, "(?i)\\bDe\\s*:")[[1]]
  parts <- lapply(parts, str_trim)
  parts <- parts[parts != ""]
  
  cleaned_body <- unlist(parts)
  cleaned_body <- paste(cleaned_body, collapse = "\n ************** \n")
  return(cleaned_body)
}

# Exemple d'utilisation
# faire un systeme plus propre de mails parents enfants ppur ne pas se retaper de la redite
# ou sinon tri par date et pas de duplicatiàon du subject
prompt_general <- 
  "Peux-tu me résumer succinctement les éléments importants de ce mail ou de cet échange
  de mail  ? Soit bref, direct et efficace, juste les infos résumées"

model_name <- "mistral-small"

date_debut <- as.Date("2024-07-19")
date_fin <- as.Date("2024-07-19")

# First, create the corps column
messages_filtres <- message_table %>%
    filter(date >= date_debut & date <= date_fin) %>%
    filter(sender_type == "personne") %>%
    mutate(subject_clean = nettoyer_sujet(subject))%>%
    arrange(desc(date))%>%
    filter(!duplicated(subject_clean))# evite les redondances

      # Create corps column from body
messages_filtres$body_clean <- sapply(messages_filtres$body, function(body) nettoyer_corps_message(body))
messages_filtres <- messages_filtres %>% filter(nchar(body_clean)!=0)
# Modify the pbapply function to use parallel processing
resultats <- pblapply(1:nrow(messages_filtres), function(i) {
  x <- messages_filtres[i, ]
  prompt <- paste(prompt_general, "\n\nSujet:", x$subject_clean, "\n\nCorps du message:", x$body_clean)
  reponse <- ask_ollama(question = prompt, model_name = model_name)
  list(
    date = x$date,
    sender = x$sender,
    subject = x$subject_clean,
    reponse = reponse
  )
  })

# Create a data frame from the results
resultats_df <- do.call(rbind, lapply(resultats, function(x) {
  data.frame(
    info = paste0("<b>", x$date, "</b><br>", x$sender, "<br>", x$subject),
    synthese = markdownToHTML(text = x$reponse, fragment.only = TRUE),
    stringsAsFactors = FALSE
  )
}))
messages_filtres%>%View()

# Modify the creation of the HTML table
html_table <- resultats_df %>%
  knitr::kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", 
                                      "condensed", "responsive"))


# Save the HTML output to a file
writeLines(html_table, "resultats_tableau.html")

# Add the final summary to the HTML output
html_table <- html_table %>%
  add_header_above(c("Résumés des messages" = 2)) %>%
  footnote(general = paste("Synthèse globale:", final_summary))

# Créer un tableau HTML avec kable et kableExtra
html_table <- resultats_df %>%
  knitr::kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", 
                                      "condensed", "responsive"))

save_kable(html_table, file = "resultats_tableau.html")

