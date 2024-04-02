# Preperations 
library(dplyr)
library(tidyr)
library(tm)
library(topicmodels)
library(ldatuning)
library(ggplot2)
load("/Data movie reviews edited.RData")

#View each review as a seperate document 
review_corpus <- Corpus(VectorSource(reviews$review_precessed))

# Transform to a word frequency matrix and remove sparsity terms that do not appear in at least 90% of the documents.
freq_matrix <- DocumentTermMatrix(review_corpus)
freq_matrix <- removeSparseTerms(freq_matrix, 0.90)

# Removing rows from the matrix which are zero
non_zero_rows <- (rowSums(as.matrix(freq_matrix)) != 0)
freq_matrix <- freq_matrix[non_zero_rows, ]

inspect(freq_matrix)

# Finding the optimal number of topics (adjust parameters as needed)
topics_df <- ldatuning::FindTopicsNumber(
  freq_matrix,
  topics = 2:25,
  metrics = "Deveaud2014",
  method = "Gibbs",
  return_models = TRUE,
  control = list(seed = 1234, iter = 2000, burnin = 4000)
)

# Plot to find the highest Deveaud metric
FindTopicsNumber_plot(topics_df) #Highest is 12

# Estimate final topic models
set.seed(1234)
lda_model <- LDA(freq_matrix, 
                 method="Gibbs", 
                 k = 12, 
                 control = list(seed = 1234, iter = 2000, burnin = 4000))
as.matrix(terms(lda_model, 10))

# Topics allocated to review
model_data <- data.frame(topics(lda_model)) %>% 
  rename(lda_results = 1)

# The end-goal is to have a dataframe which contains the review, the most likely assigned topic, and the polarity score. 

# Create a new data frame and merge with polarity scores
reviews_polarity <- merge(model_data, reviews$polarity, by = 0, all.x=TRUE) 
colnames(reviews_polarity)[colnames(reviews_polarity) == 'y'] <- 'polarity'

# Create a graph that shows how topics relate to the polarity score

# Grouping the polarity score by the topic
topic_mean_polarity <- reviews_polarity %>%
  group_by(lda_results) %>%
  summarise(mean_polarity = mean(polarity))

topic_median_polarity <- reviews_polarity %>%
  group_by(lda_results) %>%
  summarise(median_polarity = median(polarity))

# Creating a simple bar chart for mean polarity
ggplot(topic_mean_polarity, aes(x = lda_results, y = mean_polarity)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  labs(x = "Topic", y = "Mean Polarity", title = "Mean Polarity by Topic")

# Creating a simple bar chart for median polarity
ggplot(topic_median_polarity, aes(x = lda_results, y = median_polarity)) +
  geom_bar(stat = "identity", fill = ("red")) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  labs(x = "Topic", y = "Median Polarity", title = "Median Polarity by Topic")

#The dataframes have to be transformed, with the end-goal being a new dataframe that contains the number of times each topic is selected for a movie and the gross revenue for that movie
reviews <- merge(reviews, model_data, by = 0, suffixes = c("", ""))

lda_amount <- reviews %>%
  group_by(movie_id, lda_results) %>%
  summarise(amount = n()) %>%
  pivot_wider(names_from = lda_results, values_from = amount) %>%
  replace(is.na(.), 0) 

data_lm_model <- lda_amount %>% 
  left_join(moviedescriptives[, c("movie_id", "Gross")], by = "movie_id")

# Build a linear model 
revenue_lm <- lm(Gross / 10000 ~.  -movie_id, data_lm_model)
summary(revenue_lm)
