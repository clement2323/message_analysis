library(httr)
library(jsonlite)

# URL de base de l'API Ollama
base_url <- "https://ollama-clem.lab.sspcloud.fr"

# Fonction pour vérifier si un modèle est disponible
check_model_availability <- function(model_name) {
  models_response <- GET(paste0(base_url, "/api/tags"))
  if (status_code(models_response) == 200) {
    models <- content(models_response, "parsed")
    model_names <- sapply(models[[1]], function(obj) sub(":latest$", "", obj$name))
    return(model_name %in% model_names)
  }
  return(FALSE)
}

# Fonction pour pull un modèle
pull_model <- function(model_name) {
  pull_url <- paste0(base_url, "/api/pull")
  payload <- list(name = model_name)
  response <- POST(pull_url, 
                   body = toJSON(payload, auto_unbox = TRUE),
                   add_headers("Content-Type" = "application/json"),
                   encode = "json")
  
  if (status_code(response) == 200) {
    print(paste("Modèle", model_name, "téléchargé avec succès."))
    return(TRUE)
  } else {
    print(paste("Erreur lors du téléchargement du modèle:", status_code(response)))
    print(content(response, "text"))
    return(FALSE)
  }
}

ask_ollama <- function(question, model_name = "llama2", base_url = "https://ollama-clem.lab.sspcloud.fr") {
  # Vérifier si le modèle est disponible, sinon le télécharger
  if (!check_model_availability(model_name)) {
    print(paste("Le modèle", model_name, "n'est pas disponible. Tentative de téléchargement..."))
    if (!pull_model(model_name)) {
      stop("Impossible de télécharger le modèle. Arrêt de la fonction.")
    }
  }

  # Faire une requête avec le modèle
  url <- paste0(base_url, "/api/generate")
  payload <- list(
    model = model_name,
    prompt = question,
    stream = FALSE
  )

  response <- POST(url, 
                   body = toJSON(payload, auto_unbox = TRUE), 
                   add_headers("Content-Type" = "application/json"),
                   encode = "json")

  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    return(content$response)
  } else {
    stop(paste("Erreur:", status_code(response), "\n", content(response, "text")))
  }
}

format_response_html <- function(response,model_name) {
  html_template <- '
  <div style="font-family: Arial, sans-serif; max-width: 800px; margin: 20px auto; padding: 20px; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
    <h2 style="color: #333;">Réponse (%s)</h2>
    <p style="background-color: #e6f3ff; padding: 10px; border-radius: 4px; white-space: pre-wrap;">%s</p>
  </div>
  '
  formatted_html <- sprintf(html_template,model_name, response)
  return(formatted_html)
}

reponse <- ask_ollama("qui ne pete ni rote est voué à explosion. qu'en pense-tu  ?, réponse en rimes",
model_name = "mistral-small"
)

# Install and load the htmltools package if not already installed
library(htmltools)

# Use save_html from htmltools to save the formatted response
htmltools::save_html(HTML(format_response_html(reponse, "mistral-small")), file = "reponse.html")
# Exemple d'utilisation :
# 

reponse <- ask_ollama("qu'est ce qu'un tracteur, réponse en haiku",model_name = "mistral-small")
# print(reponse)
