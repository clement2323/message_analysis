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

messages_septembre <- message_table %>%
  filter(date >= as.Date("2024-09-15") & date <= as.Date("2024-09-20")) 

message <- messages_septembre$body[8]

resumer_message_llm <- function(message, nom_model ="tiny-llama-sum") {
  prompt <- paste0("resume moi ce message : ", message)
  rep_df <- generate(nom_model, prompt, output = "df")
  return(rep_df$response[1])
}

out <- lapply(messages_septembre$body[20:40],resumer_message_llm)

messages_septembre$body[49]
# Example usage:
resume <- resumer_message_llm("coucou c'est moi")
print(resume)
