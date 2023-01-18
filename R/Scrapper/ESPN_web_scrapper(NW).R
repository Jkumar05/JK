library(rvest)

#read web page
keyword = "kohli"

url = paste0("https://www.espn.in/search/_/q/dhoni")
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes("span")%>%
  html_text()
News

#Extracting Discription
Discription = url %>%
  html_nodes(".Truncate Truncate--collapsed span")%>%
  html_text()
Discription

#Extracting Links
links = url %>%
  html_nodes("a")%>%
  html_attr("href")
links

#Extracting Date
Date = url %>%
  html_nodes(".SrcLst_pst ul")%>%
  html_text()

#Creating a dataframe
NDTV = as.data.frame(cbind(Date,News,Discription,links))
View(NDTV)
