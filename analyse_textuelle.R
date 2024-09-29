message_table_text <- message_table %>%
    select(id,sender,body)

tidy_bodies <- message_table_text%>%
  unnest_tokens(word,body)

# Fonction pour nettoyer le texte
clean_text <- function(text) {
  text %>%
    # Supprimer complètement la partie avant l'apostrophe pour tous les cas
    str_replace_all("\\b\\w+['’](\\w+)", "\\1") %>%
    # ... le reste de la fonction reste inchangé ...
    str_replace_all("-", " ") %>%
    str_to_lower()
}
tidy_bodies <- message_table_text %>%
  unnest_tokens(word, body) %>%
  mutate(word = clean_text(word))

tidy_bodies %>% 
  anti_join(stopwords_df, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  head(200) %>% 
  pull(word) 
#stop words 
french_stopwords <- stopwords("fr", source = "snowball")
stopwords_df <- data.frame(word = french_stopwords)

# Liste de stopwords supplémentaires spécifiques
bruit <- c("sr971","dr69","tr","to","https", "objet", "clément", "a", "jean", "guillo", 
                   "cc", "re", "tél", "dir", "luc","tous", "go.techtarget.com", 
                   "di971", "ci", "d'un", "dg", "www.insee.fr", 
                   "c'est", "dr", "répondre", "favreau","fr","r","dg75",
                   "insee","dg75","si","plus","bien","bonjour","merci","cordialement" # commodités
                   ,"envoyé","bonjour","cordialement"
                   )
jour_semaine <-c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
prenoms <- c("patrick","baptiste","philippe","marie","xavier","benhaddouche","bertrand","sébastien",
"louis","emmanuel","marie","françois","herbet","sophie","ali",
"annaïck","francois","claire","clovis","guylaine","camille","yves","ménard")

stopwords_sup <- c(bruit,jour_semaine,prenoms)

# Fonction pour ajouter des stopwords basés sur un pattern regex
add_regex_stopwords <- function(current_stopwords, pattern, data, description) {
  new_stopwords <- grep(pattern, data$word, value = TRUE) %>% unique()
  message(paste("Ajout de", length(new_stopwords), "stopwords pour:", description))
  return(c(current_stopwords, new_stopwords))
}

# Regex pour les jours et les mois
pattern_jour_mois <- "^\\d+$|^(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre)$"
stopwords_sup <- add_regex_stopwords(stopwords_sup, pattern_jour_mois, tidy_bodies, "jours et mois")

# Regex pour les mots avec un point au milieu
pattern_avec_point <- "^[a-z]+\\.[a-z]+$"
stopwords_sup <- add_regex_stopwords(stopwords_sup, pattern_avec_point, tidy_bodies, "mots avec un point")

# mailto
pattern_mailto <- "^mailto:.+$"
stopwords_sup <- add_regex_stopwords(stopwords_sup, pattern_mailto, tidy_bodies, "adresses mailto")

# .fr..
pattern_fr_domains <- "\\.fr$"
stopwords_sup <- add_regex_stopwords(stopwords_sup, pattern_fr_domains, tidy_bodies, "domaines se terminant par .fr")


# Combinaison de tous les stopwords
stopwords_df <- bind_rows(stopwords_df, data.frame(word = stopwords_sup))

# Affichage des mots les plus fréquents après filtrage
tidy_bodies_clean <- tidy_bodies %>% 
  anti_join(stopwords_df, by = "word")



# Compter la fréquence des mots
word_counts <- tidy_bodies_clean %>%
  count(word, sort = TRUE)

word_counts%>% View()
# Créer le nuage de mots
set.seed(1234) # Pour la reproductibilité
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 2,
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# Sauvegarder le wordcloud dans un fichier
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 2,
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))
dev.off()