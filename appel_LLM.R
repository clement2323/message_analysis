
library(ollamar)
Sys.setenv(no_proxy = "localhost,127.0.0.1") # ollama run sur le 11434..
Sys.unsetenv("http_proxy") # remttre les proxy insee pour installation de package seulement
Sys.unsetenv("https_proxy")
test_connection()  # test connection to Ollama server
# bon pour installer les packages mais mauvais pour bosser en local

# le pull marche pas à cause des proxy mais avec le create sur un gguf pers c'est cool
list_models() #models à installe à la mano avec llama create et parant d'ungguf model file etc..

# generate a response/text based on a prompt; returns an httr2 response by default
resp <- generate("tiny-llama", "tell me a story") 
#resp_process(resp, "text") 
#resp_process(resp, "df") 
rep_df <- generate("tiny-llama", "donne moi un compliment", output = "df")

rep_df$response
message_table%>% select(date) %>% head()




# Fonction R pour pull un modèle Ollama via le proxy INSEE
pull_ollama_model <- function(model_name) {
  proxy_command <- '
  $env:HTTP_PROXY = "http://proxy-rie.http.insee.fr:8080"
  $env:HTTPS_PROXY = "http://proxy-rie.http.insee.fr:8080"
  $env:NO_PROXY = "localhost,127.0.0.1"
  ollama pull {model_name}
  '
  proxy_command <- gsub("{model_name}", model_name, proxy_command)
  system2("powershell", args = c("-Command", proxy_command))
}

# Utilisation
# Fonction R pour pull un modèle Ollama via le proxy INSEE


pull_ollama_model("mistral-small")
