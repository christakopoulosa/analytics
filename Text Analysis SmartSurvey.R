##Import survey data
library(data.table)
mydata = fread("417016.csv", col.names = c("userID", "userNo", "name", "email", "ip","uniqueID", "started", "ended",
                                           "satisfactionLevel", "improvements", "task", "taskLevelOfDifficulty" 
                                           , "taskLevelOfDifficultyReason"));mydata

##Break text data to tokens (one-token-per-document-per-row)
library(dplyr)
library(tidytext)
improvements <- data_frame(improvements = mydata$improvements);improvements
taskLevelOfDifficultyReason <- data_frame(taskLevelOfDifficultyReason = mydata$taskLevelOfDifficultyReason);


improvements <- improvements %>%
  unnest_tokens(word, improvements);improvements

taskLevelOfDifficultyReason <- taskLevelOfDifficultyReason %>%
  unnest_tokens(word, taskLevelOfDifficultyReason);taskLevelOfDifficultyReason


##Remove stop words using the stop_words dataset given by tidytext
data("stop_words") 
improvements <- improvements %>%
  anti_join(stop_words)

taskLevelOfDifficultyReason <- taskLevelOfDifficultyReason %>%
  anti_join(stop_words)

##Check the most common words in the data set
improvements %>%
  count(word, sort = TRUE)

taskLevelOfDifficultyReason %>%
  count(word, sort = TRUE)


##Visualize the most common words
library(ggplot2)

improvements %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##Sentiment analysis
improvements <- improvements %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

##Plot negative and positive sentiments
improvements %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Check the level of dissatisfaction with afinn lexicon
taskLevelOfDifficultyReason <- taskLevelOfDifficultyReason %>%
  inner_join(get_sentiments("afinn"))

#Wordcloud with coloroured positive and negative words
library(wordcloud)
library(reshape2)

improvements %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
