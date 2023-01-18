library(rvest)
library(sentimentr)
library(ggplot2)

#read web page
keyword = "cancun"

url = paste0("https://lucesdelsiglo.com/?s=",keyword)
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes(".entry-title.td-module-title")%>%
  html_text()
News = News[-c(1,2,3,4)]
News

#Extracting Description
Description = url %>%
  html_nodes(".td-excerpt")%>%
  html_text()
Description = trimws(Description)
Description = str_remove_all(Description, "[\r]")
Description = str_remove_all(Description, "[\n]")
Description

#Extracting Links
links = url %>%
  html_nodes(".td-module-title a")%>%
  html_attr("href")
links= links[-c(1,2,3,4)]
links

#Extracting Date
Date = url %>%
  html_nodes(".td-module-date")%>%
  html_text()
Date

#Creating a dataframe
LUCAS = as.data.frame(cbind(Date,News,Description,links))
View(LUCAS)
