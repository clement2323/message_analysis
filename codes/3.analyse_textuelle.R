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

tidy_bodies <- tidy_bodies %>%
  mutate(word = clean_text(word))

#stop words 
french_stopwords <- stopwords("fr", source = "snowball")
stopwords_df <- data.frame(word = french_stopwords)

# Liste de stopwords supplémentaires spécifiques
bruit <- c("sr971","dr69","tr","to","https", "objet", "clément", "a", "jean", "guillo", 
                   "cc", "re", "tél", "dir", "luc","tous", "go.techtarget.com", 
                   "di971", "ci", "d'un", "dg", "www.insee.fr", 
                   "est", "dr", "répondre","fr","r","dg75",
                   "insee","dg75","si","plus","bien","merci","cordialement" # commodités
                   ,"envoyé","bonjour","cordialement","bonne","journée","the","entre","joint","deux"
            )
jour_semaine <-c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
prenoms <- c("patrick","baptiste","philippe","marie","xavier",
"benhaddouche","bertrand","sébastien","louis","emmanuel","marie","françois",
"herbet","sophie","ali","isabelle","dussud","michel","herbet","christine","pierre",
"of", "frédéric","où","ça","durand","annaïck","francois","claire","clovis","guylaine","camille",
"yves","ménard","favreau","dorelon","aumand","rouvière","annick","durand","clarenc","chantal","jonny","yengadessin")


mois <- c("janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre")
mots_courants <- c("caix","direction","régionale","guyane","guadeloupe","martinique","maim","cécile","sous","après","cliquer","vers","être")

sender_names <- lapply(message_table$full_name, extract_name)
sender_prenoms <- sapply(sender_names, function(x) x$prenom)%>% tolower()
sender_noms <- sapply(sender_names, function(x) x$nom)%>% tolower()

stopwords_sup <- c(bruit,jour_semaine,mois,prenoms,mots_courants,sender_noms,sender_prenoms)

# Combinaison de tous les patterns en un seul
pattern_combine <- paste0(
  "\\d|",                  # Chaînes contenant des chiffres
  "^[a-z]+\\.[a-z]+$|",    # Mots avec un point au milieu
  "^mailto:.+$|",          # Adresses mailto
  "\\.fr$|",               # Domaines se terminant par .fr
  "\\.com"                 # Chaînes contenant .com
)

stopwords_sup <- add_regex_stopwords(stopwords_sup, pattern_combine, tidy_bodies, "tous les patterns combinés")

# Combinaison de tous les stopwords
stopwords_df <- bind_rows(stopwords_df, data.frame(word = stopwords_sup))

# Affichage des mots les plus fréquents après filtrage
tidy_bodies_clean <- tidy_bodies %>% 
  anti_join(stopwords_df, by = "word")

# stats desc
creer_nuage_mots(tidy_bodies_clean)
creer_nuage_mots(tidy_bodies_clean,"patrick.favreau@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"louis.malard@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"annick.durand@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"philippe.clarenc@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"philippe.dorelon@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"bertrand.aumand@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"luc.rouviere@insee.fr")
creer_nuage_mots(tidy_bodies_clean,"jean-baptiste.herbet@insee.fr")

