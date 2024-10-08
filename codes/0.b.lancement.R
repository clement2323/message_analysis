rm(list=ls())

# Chargement fonctions avec encodage UTF-8
source("codes/fonctions/fonctions_utiles.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graphiques.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_graph.R", encoding = "UTF-8")
source("codes/fonctions/fonctions_appel_ollama.R", encoding = "UTF-8")

# chargement setup
source("codes/0.a.setup.R", encoding = "UTF-8")
source("codes/1.preparation_donnees.R", encoding = "UTF-8")

# Function to render RMarkdown file with global environment

rmarkdown::render(
    input = "codes/presentation.rmd",
    output_format = "html_document",
    envir = globalenv()
  )

