rm(list=ls())

source("codes/0.a.setup.R", encoding = "UTF-8")
source("codes/0.b.metadonnees.R", encoding = "UTF-8")

# Chargement fonctions avec encodage UTF-8
source("codes/fonctions/fonctions_utiles.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graphiques.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graph.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_appel_ollama.R", encoding = "UTF-8")

# chargement setup
source("codes/1.preparation_donnees.R", encoding = "UTF-8")



# Function to render RMarkdown file with global environment
rmarkdown::render(
    input = "codes/presentation.rmd",
    output_format = "html_document",
    envir = globalenv()
  )


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
-
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