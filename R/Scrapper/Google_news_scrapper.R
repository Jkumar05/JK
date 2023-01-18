library(robotstxt)
library(sentimentr)
library(rvest)
library(ggplot2)

#robotstxt
paths_allowed(
  paths = c("https://news.google.com/search?q=dhoni&hl=en-IN&gl=IN&ceid=IN%3Aen")
)

#read web page
keyword = "dhoni"
time = "1"
interval = "d"
url = paste0("https://news.google.com/search?q=",keyword,"%20when%3A",time,interval,"&hl=en-IN&gl=IN&ceid=IN%3Aen")
url = read_html(url)

#Extracting News
news = url %>%
  html_nodes("h3")%>%
  html_text()

#Extracting Links
links = url %>%
  html_nodes(".VDXfz")%>%
  html_attr("href")%>%
  paste("https://news.google.com",.,sep = "")

x= as.data.frame(news,links)
View(x)

#calculating the sentiment
s = sentiment_by(x$news,by = NULL,
                 averaging.function = sentimentr::average_downweighted_zero,
                 group.names)
s
#creating a column Key with true if ave_sentiment>0 else false
Key = s$ave_sentiment>0

#binding the column Key with the sentiment dataframe
z = cbind(s,Key)
z = as.data.frame(z)
z[z == "TRUE"] = "Positive"
z[z == "FALSE"] = "Negative"
z

#plotting graph based of the sentiment analysis
ggplot(z, aes(x = element_id, y = ave_sentiment, fill = Key)) +
  geom_col(position = "identity")+
  scale_x_continuous(name="News Number", breaks =seq(0,100,1)) +
  scale_y_continuous(name="Sentiment")


