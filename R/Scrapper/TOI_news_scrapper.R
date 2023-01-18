library(rvest)
library(sentimentr)
library(ggplot2)

#read web page
keyword = "kohli"

url = paste0("https://timesofindia.indiatimes.com/topic/",keyword,"/news")
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes("#c_wdt_list_1 .w_tle a")%>%
  html_text()

#Extracting Links
links = url %>%
  html_nodes("#c_wdt_list_1 .w_tle a")%>%
  html_attr("href")%>%
  paste("https://timesofindia.indiatimes.com",.,sep = "")

#Extracting Date
Date = url %>%
  html_nodes(".art_date")%>%
  html_text()

#Creating a dataframe
TOI = as.data.frame(cbind(Date,News,links))
View(TOI)

#calculating the sentiment
s = sentiment_by(TOI$News,by = NULL,
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
  scale_x_continuous(name="News Number", breaks =seq(0,50,1))+
  scale_y_continuous(name="Sentiment")

