library(tidyverse)
library(rtweet)
library(ggthemes)
library(tm)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(ggpubr)
# Read in the data----------------------
tweets.df <- read_csv("elonmusk.csv")
dim(tweets.df) # a very large dataset!,
names(tweets.df)
tweets.df$text
tweets.df$UserScreenName


### Plot the daily time series--------------
tweets.df %>%
  mutate(created_at=ym(substr(Timestamp, 1, 7))) %>% # extracts only the year and month
  # from the date/time variable
  group_by(UserScreenName) %>% # group by candidate
  count(created_at, "%Y-%M-%D") %>% # counts the number of tweets created each day
  ggplot(aes(x=as.Date(created_at, "%Y-%M-%D"), y=n, color=UserScreenName, group = UserScreenName)) +
  geom_line() +
  labs(x="Date",
       y="Number of Tweets",
       title = "Daily frequency of tweets posted by candidates",
       subtitle = "Tweets from Elon Musk's twitter, including tweets and replies to others",
       caption = "Tweets collected between the year 2010 and the year 2022",
       color = "Candidates") +
  scale_x_date(date_labels = "%b %Y") +
  scale_colour_manual(values = c("Elon Musk" = "red")) +
  theme_bw() +
  theme(legend.position="bottom")


# Daily frequency of Comments to posts by candidates--------------------------------------------------------

tweets.df$Comments <- as.numeric(gsub(",","",tweets.df$Comments)) # gsub() will return a character vector, not a numeric vector.
#as.numeric will convert the character vector back into numeric

tweets.df %>%
  mutate(created_at=ymd(substr(Timestamp, 1, 10))) %>% # extracts only the date
  # from the date/time variable
  group_by(UserScreenName,created_at) %>% # group by candidate and date
  summarise(tot_Comments=sum(Comments)) %>% # counts the number of Comments each day
  ggplot(aes(x=as.Date(created_at), y=tot_Comments, color=UserScreenName, group = UserScreenName)) +
  geom_line() +
  labs(x="Date",
       y="Total Comments",
       title = "Daily frequency of Comments to posts by candidates",
       subtitle = "Tweets from Elon Musk's twitter, including tweets and replies to others",
       caption = "Tweets collected between the year 2010 and the year 2022",
       color = "Candidates") +
  scale_x_date(date_labels = "%b %Y") +
  scale_colour_manual(values = c("Elon Musk" = "orange")) +
  theme_bw() +
  theme(legend.position="bottom")


# Daily frequency of Likes to posts by candidates--------------------------------------------------------

tweets.df$Likes <- as.numeric(gsub(",","",tweets.df$Likes)) # gsub() will return a character vector, not a numeric vector.
#as.numeric will convert the character vector back into numeric

tweets.df %>%
  mutate(created_at=ymd(substr(Timestamp, 1, 10))) %>% # extracts only the date
  # from the date/time variable
  group_by(UserScreenName,created_at) %>% # group by candidate and date
  summarise(tot_Likes=sum(Likes)) %>% # counts the number of Likes each day
  ggplot(aes(x=as.Date(created_at), y=tot_Likes, color=UserScreenName, group = UserScreenName)) +
  geom_line() +
  labs(x="Date",
       y="Total Likes",
       title = "Daily frequency of Likes to posts by candidates",
       subtitle = "Tweets from Elon Musk's twitter, including tweets and replies to others",
       caption = "Tweets collected between the year 2010 and the year 2022",
       color = "Candidates") +
  scale_x_date(date_labels = "%b %Y") +
  scale_colour_manual(values = c("Elon Musk" = "blue")) +
  theme_bw() +
  theme(legend.position="bottom")

# Daily frequency of Retweets to posts by candidates--------------------------------------------------------

tweets.df$Retweets <- as.numeric(gsub(",","",tweets.df$Retweets)) # gsub() will return a character vector, not a numeric vector.
#as.numeric will convert the character vector back into numeric

tweets.df %>%
  mutate(created_at=ymd(substr(Timestamp, 1, 10))) %>% # extracts only the date
  # from the date/time variable
  group_by(UserScreenName,created_at) %>% # group by candidate and date
  summarise(tot_retweet=sum(Retweets)) %>% # counts the number of retweets each day
  ggplot(aes(x=as.Date(created_at), y=tot_retweet, color=UserScreenName, group = UserScreenName)) +
  geom_line() +
  labs(x="Date",
       y="Total Retweets",
       title = "Daily frequency of retweets to posts by candidates",
       subtitle = "Tweets from Elon Musk's twitter, including tweets and replies to others",
       caption = "Tweets collected between the year 2010 and the year 2022",
       color = "Candidates") +
  scale_x_date(date_labels = "%b %Y") +
  scale_colour_manual(values = c("Elon Musk" = "green")) +
  theme_bw() +
  theme(legend.position="bottom")



### Data Clean-Up--------------------------------------------
###
#
# Looking at the data above, it becomes clear that there is a lot of clean-up associated
# with social media data.
en_tweets <- tweets.df %>%
  filter(UserName=="@elonmusk")

en_tweets$stripped_text <- gsub("http.*","",  en_tweets$text)
en_tweets$stripped_text <- gsub("https.*","", en_tweets$stripped_text)
en_tweets$stripped_text <- gsub("amp","", en_tweets$stripped_text)
en_tweets$stripped_text <- gsub("RT","", en_tweets$stripped_text)
en_tweets$stripped_text <- stripWhitespace(en_tweets$stripped_text)
en_tweets$stripped_text <- removeNumbers(en_tweets$stripped_text)
head(en_tweets$stripped_text)


#----------------------------------------------------------
# Elon Musk Tweets
Elon_Musk_tweets <- en_tweets %>%
  filter(UserScreenName=="Elon Musk")
#
# Remove punctuation, convert to lowercase, add id for each tweet:
Elon_Musk_tweets_clean <- Elon_Musk_tweets %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, stripped_text)

head(Elon_Musk_tweets_clean)

#----------------------------------------------------------
# remove stop words from your list of words
Elon_Musk_cleaned_words <- Elon_Musk_tweets_clean %>%
  anti_join(stop_words) # return all rows where there are not matching values
# in stop_words
#
# Define our own stopwords that we don't want to include
my_stop_words <- data.frame(word = c("elon", "replying"))
#
# remove our own stopwords from the list of words too
Elon_Musk_cleaned_words_2 <- Elon_Musk_cleaned_words %>%
  anti_join(my_stop_words)

#
# plot the top 10 words
Elon_Musk_cleaned_words_2 %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill=word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = rainbow(10)) +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 words in Elon Musk's tweets") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 12))


#-----------------------------------------------------------
### Elon Musk
#
# Now for the wordcloud, calculate the count of each word and sort the words
# according to the count.
# Then, calculate the frequency of each word, as
# the count of each word / the total count
#
Elon_Musk_cleaned_words_3 <- Elon_Musk_cleaned_words_2 %>%
  count(word, sort = TRUE) %>%
  mutate(freq = n / sum(n))

# Wordcloud of Elon Musk’s Tweets:
# Create a wordcloud
#
with(Elon_Musk_cleaned_words_3,
     wordcloud(word, freq,
               min.freq = 1,
               max.words = 100,
               random.order = FALSE,
               colors = brewer.pal(8, "Dark2"),
               scale = c(4.5, 0.1)))

#-----------------------------------------------------------
### Elon Musk
#
# Join sentiment classification to the tweet words
Elon_Musk_bing <- Elon_Musk_cleaned_words_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#
# Finally, plot top words, grouped by positive vs. negative sentiment.
#
Elon_Musk_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = rainbow(3)) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Elon Musk's most common Positive and Negative words",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 10))


#---------------------------------------------------------------

#
# We count up how many positive and negative words there are in each tweet
#
# associate sentiment scores to each tweet
Elon_Musk_sentiment <- Elon_Musk_cleaned_words_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweetnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment
  # in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
#
#
# Let's work out the mean score
# We'll include it as a line and as a numerical value to our plot
#
sentiment_means_Elon_Musk <- Elon_Musk_sentiment %>%
  summarize(mean_score = mean(score))
#
# Barplot
#
ggplot(Elon_Musk_sentiment,aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgreen", colour = "darkgreen") + # geom_bar will do the
  # tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means_Elon_Musk) +
  # Add a vertical line at the mean score, calculated and stored in
  # sentiment_means_Elon_Musk above
  geom_text(aes(x = mean_score,
                y = Inf,
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2,
            data = sentiment_means_Elon_Musk) +
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10, # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers;
  # set this to a suitably large range
  labs(title = paste("Sentiments in Elon Musk's tweets give a mean of",
                     signif(sentiment_means_Elon_Musk$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three
       # significant figures
       x = "Sentiment Score",
       y = "Number of tweets") +
  theme(title = element_text(size = 10))

# bing--------------------------------------
bing_vector <- Elon_Musk_cleaned_words_2 %>%
  inner_join(get_sentiments("bing"))
head(bing_vector)
summary(bing_vector)


# Perform a t.test
test <- t.test(Elon_Musk_sentiment$score ~ 1, data = Elon_Musk_sentiment)
test

# Display t-test result
cat("t.test p.value:", test$p.value, "\n")
cat("t.test estimate (meanof x):", test$estimate, "\n")
cat("t.test confidence interval:", test$conf.int, "\n")

# Check the number of observations in the data
n_obs <- length(Elon_Musk_sentiment$score)
n_obs

# Perform the Shapiro-Wilk test only if the sample size is between 3 and 5000
if (n_obs >= 3 && n_obs <= 5000) {
  shapiro.test(Elon_Musk_sentiment$score)
} else {
  warning("Sample size: ",n_obs," is not within the valid range for the Shapiro-Wilk test.")
}

# Check normality
ggboxplot(Elon_Musk_sentiment$score, ylab = "Frequency", xlab = FALSE, ggtheme = theme_minimal())
ggqqplot(Elon_Musk_sentiment$score, ylab = "Frequency", ggtheme = theme_minimal())





