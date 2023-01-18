library(rvest)
library(sentimentr)
library(ggplot2)

#read web page
keyword = "cancun"

url = paste0("https://www.poresto.net/noticias/buscar/?buscar=",keyword)
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes(".titulo a")%>%
  html_text()
News

#Extracting Description
Description = url %>%
  html_nodes("#resultados p")%>%
  html_text()
Description

#Extracting Links
links = url %>%
  html_nodes("#resultados a")%>%
  html_attr("href")%>%
  paste("https://www.poresto.net",.,sep = "")
links

#Extracting Date
Date = url %>%
  html_nodes(".titulo-grupo")%>%
  html_text()
Date

Date_width = url %>%
  html_nodes(".caja,.titulo-grupo")%>%
  html_text()
Date_width

#Creating a dataframe
PORESTO = as.data.frame(cbind(News,Description,links))
View(PORESTO)
