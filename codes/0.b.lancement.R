rm(list=ls())

# Chargement fonctions avec encodage UTF-8
source("codes/fonctions/fonctions_utiles.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graphiques.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graph.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_appel_ollama.R", encoding = "UTF-8")

# chargement setup
source("codes/0.a.setup.R", encoding = "UTF-8")
source("codes/1.preparation_donnees.R", encoding = "UTF-8")

traiter_messages <- function(date_debut, date_fin, prompt_general, model_name = "mistral-small") {
  # Filtrer les messages
  messages_filtres <- filtrer_messages(date_debut, date_fin)
  
  # Traiter chaque message
  resultats <- pblapply(1:nrow(messages_filtres), function(i) {
    traiter_message_individuel(messages_filtres[i,], prompt_general, model_name)
  })
  
  return(resultats)
}

filtrer_messages <- function(date_debut, date_fin) {
  messages_filtres <- message_table %>%
    filter(date >= date_debut & date <= date_fin)
  
  messages_filtres$subject_clean <- sapply(messages_filtres$subject, nettoyer_sujet)
  
  return(messages_filtres)
}

traiter_message_individuel <- function(message, prompt_general, model_name) {
  message_content <- message$body
  sujet <- message$subject_clean
  
  cleaned_body <- nettoyer_corps_message(message_content)
  
  parts <- str_split(cleaned_body, "(?i)\\bDe\\s*:")[[1]]
  parts <- lapply(parts, str_trim)
  parts <- parts[parts != ""]
  
  result <- Reduce(function(x, y) paste(x, paste(rep("*", 100), collapse=""), y, sep="\n"), parts)
  
  requete_ollama <- paste0(prompt_general, "Sujet : ", sujet, "mails : ", result)
  reponse <- ask_ollama(requete_ollama, model_name = model_name)
  
  return(list(sujet = sujet, reponse = reponse))
}

nettoyer_corps_message <- function(message_content) {
  cleaned_body <- message_content %>%
    str_replace_all("\\b(\\+?\\d{1,4}[-.\\s]?)?\\(?\\d{1,4}\\)?[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,9}\\b", "") %>%
    str_replace_all("\\b[\\w\\s]+\\s+<[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}(\\s*<mailto:[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,4}>)?\\s*>", "") %>%
    str_replace_all("\\b[A-Z][a-z]+\\s+[A-Z][a-z]+[-]?;", "") %>%
    str_replace_all(";\\s*$", "") %>%
    str_replace_all(";\\s*\n", "\n") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    gsub(pattern = ";", replacement = "")
  
  return(cleaned_body)
}

# Exemple d'utilisation
date_debut <- as.Date("2024-09-22")
date_fin <- as.Date("2024-09-23")
prompt_general <- "Peux-tu me synthétiser les éléments importants de ce mail ou échange de mail selon les cas ? Sers-toi du sujet aussi. N'hésite pas à me dire si je dois répondre de suite ou non, ou si c'est juste informatif pour moi."
resultats <- traiter_messages(date_debut, date_fin, prompt_general)

resultats_df <- data.frame(
  Résumé = sapply(resultats, function(x) markdownToHTML(text = x$reponse, fragment.only = TRUE)),
  stringsAsFactors = FALSE
)

#write.csv(resultats_df, file = "resultats_synthese.csv", row.names = FALSE, fileEncoding = "UTF-8")
# Créer un tableau HTML avec kable et kableExtra
html_table <- resultats_df %>%
  knitr::kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

save_kable(html_table, file = "resultats_tableau.html")


## faire une dernière couche de ollama par dessus pour résumer le résumé ? et prioriser ordionén , fair eun beau document quoi
### regarder le nombre de message reçus sur une semaine ça peut prendre du temps


liste_paquets <- split(resultats_df$Résumé,seq_along(resultats_df$Résumé)%%6)

prompt_synthese <- "fais une synthese à partir de la synthèse générale précédente et 
des paquets de mails que je t'ajoute
si c'est une newletter ou quelque chose de très secondaire, 
(accès siamois garantis, histoire de pointages, bôite pleine, ne l'integre pas à la synthèse)
"

synthese_prec <-""

print(paste0("n paquets = ",length(liste_paquets)))
compteur = 1
for(paquet in liste_paquets){

    #paquet <- liste_paquets[[1]]
    print(paste0("paquet numero : ",compteur))
    paquet <- paste0(paquet,collapse="")
    prompt <- paste0(prompt_synthese, "Synhese Précédente :", synthese_prec,"paquet mails :" ,paquet)
    reponse <- ask_ollama(prompt, model_name = "mistral-small")   
    synthese_prec <- reponse
    compteur = compteur + 1
}


res_final_df <- data.frame(synthese = markdownToHTML(text = synthese_prec, fragment.only = TRUE))%>%
    knitr::kable(escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

res_final_df

save_kable(res_final_df, file = "synthese_de_synthese.html")
