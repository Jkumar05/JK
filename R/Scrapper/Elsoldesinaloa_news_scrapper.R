library(rvest)
library(sentimentr)
library(ggplot2)

#read web page
keyword = "cancun"

url = paste0("https://www.elsoldesinaloa.com.mx/buscar/?q=",keyword)
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes("#tab-story .title a")%>%
  html_text()
News

#Extracting Links
links = url %>%
  html_nodes("#tab-story .title a")%>%
  html_attr("href")
links

#Extracting Date
Date = url %>%
  html_nodes("#tab-story .timestamp")%>%
  html_text()
Date = trimws(Date)
Date

#Creating a dataframe
elsoldesinaloa = as.data.frame(cbind(Date,News,links))
View(elsoldesinaloa)
