library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)

data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

## beta - probability of a term in a topic
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

## gamma - probability of a topic in a document
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents