
# Fonction pour vérifier si un modèle est disponible
check_model_availability <- function(model_name,base_url = "https://ollama-clem.lab.sspcloud.fr") {
  models_response <- GET(paste0(base_url, "/api/tags"))
  if (status_code(models_response) == 200) {
    models <- content(models_response, "parsed")
    model_names <- sapply(models[[1]], function(obj) sub(":latest$", "", obj$name))
    return(model_name %in% model_names)
  }
  return(FALSE)
}

# Fonction pour pull un modèle
pull_model <- function(model_name,base_url = "https://ollama-clem.lab.sspcloud.fr") {
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
  if (!check_model_availability(model_name,base_url)) {
    print(paste("Le modèle", model_name, "n'est pas disponible. Tentative de téléchargement..."))
    if (!pull_model(model_name,base_url)) {
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

