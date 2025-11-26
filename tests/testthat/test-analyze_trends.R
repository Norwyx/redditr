library(testthat)
library(tidyverse)
library(tidytext)

mock_reddit_tibble <- tibble(
  title = c(
    "An Excellent Programming Test",
    "Another test, I really love programming, but the test is hard"
  ),
  selftext = c(
    "This is a long sentence. The python library is good. The library is very good.",
    "The python test is not fun. Test, test, test. I hate writing tests."
  )
)
# Expected Frequencies: 'test': 6, 'programming': 2, 'python': 2, 'library': 2, 'good': 2

test_that("analyze_trends returns a list of two ggplot objects", {
  result <- analyze_trends(mock_reddit_tibble)
  
  expect_type(result, "list")
  expect_named(result, c("frequency_plot", "sentiment_plot"))
  
  # Check that both elements are ggplot objects
  expect_s3_class(result$frequency_plot, "ggplot")
  expect_s3_class(result$sentiment_plot, "ggplot")
})

test_that("analyze_trends sentiment analysis is correct", {
  
  # Expected Sentiment from mock_reddit_tibble (using bing lexicon):
  # 'great', 'love', 'good' (3 positive)
  # 'hard', 'hate' (2 negative)
  
  result <- analyze_trends(mock_reddit_tibble)
  
  # Extract data used to create the sentiment plot
  sentiment_data <- ggplot_build(result$sentiment_plot)$data[[1]] %>%
    select(x, y) %>%
    mutate(sentiment = ifelse(x == 1, "negative", "positive")) # Assuming alphabetical order
  
  # Check positive count (should be 3)
  positive_count <- sentiment_data %>% filter(sentiment == "positive") %>% pull(y)
  
  # Check negative count (should be 2)
  negative_count <- sentiment_data %>% filter(sentiment == "negative") %>% pull(y)
  
  expect_equal(positive_count, 3)
  expect_equal(negative_count, 2)
})