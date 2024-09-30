# bon pour installer les packages mais mauvais pour bosser en local
Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

packages <- c("jsonlite", "dplyr", "lubridate", 
"stringr", "tidyr", "ggplot2", "ggiraph","gganimate",
 "visNetwork","tidytext","wordcloud","RColorBrewer","httr2","httr","kableExtra","pbapply","markdown")

sapply(packages,installer_package)
sapply(packages, require, character.only = TRUE)
# Load the rollama package and ping Ollama to ensure connectivity.

