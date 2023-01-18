library(rvest)
library(sentimentr)
library(ggplot2)

#read web page
keyword = "chennai+lockdowm"

url = paste0("https://sports.ndtv.com/search?q=",keyword)
url = read_html(url)

#Extracting News
News = url %>%
  html_nodes(".SrcLst_ttl")%>%
  html_text()

#Extracting Discription
Discription = url %>%
  html_nodes(".SrcLst_txt-wrp p")%>%
  html_text()

#Extracting Links
links = url %>%
  html_nodes(".SrcLst_ttl")%>%
  html_attr("href")

#Extracting Date
Date = url %>%
  html_nodes(".SrcLst_pst ul")%>%
  html_text()

#Creating a dataframe
NDTV = as.data.frame(cbind(Date,News,Discription,links))
View(NDTV)

#calculating the sentiment
s = sentiment_by(NDTV$Discription,by = NULL,
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
  scale_x_continuous(name="News Number", breaks =seq(0,10,1)) +
  scale_y_continuous(name="Sentiment")

