Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

rm(list=ls())

packages <- c("jsonlite", "dplyr", "lubridate", "stringr", "tidyr", "ggplot2", "ggiraph","gganimate")
sapply(packages, require, character.only = TRUE)