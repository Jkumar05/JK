#importing libraries
library(sentimentr)
library(ggplot2)

#importing file
file = read.csv("C:/Users/vella/Downloads/kohli.csv")

#calculating the sentiment
s = sentiment_by(file$Description,by = NULL,
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

