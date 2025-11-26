library(testthat)
library(tidyverse)
library(tidytext)

mock_reddit_tibble <- tibble(
  title = c(
    "A Great Programming Test",
    "Another test, I really love programming, but the test is hard"
  ),
  selftext = c(
    "This is a long sentence. The python library is good. The library is very good.",
    "The python test is not fun. Test, test, test. I hate writing tests."
  )
)
# Expected Frequencies: 'test': 6, 'programming': 2, 'python': 2, 'library': 2, 'good': 2

test_that("top_words returns correct structure and respects n limit", {
  result <- top_words(mock_reddit_tibble, n = 3)
  
  expect_true(is_tibble(result))
  expect_equal(colnames(result), c("word", "n"))
  expect_equal(nrow(result), 3) 
})


test_that("top_words correctly filters stopwords and counts frequency", {
  result_top1 <- top_words(mock_reddit_tibble, n = 1)
  
  # The highest frequency word >= 3 chars, NOT a stopword, is 'test' (6)
  expect_equal(result_top1$word[1], "test")
  expect_equal(result_top1$n[1], 7)
  
  # Check that common stopwords ('the', 'is') are filtered out
  result_all <- top_words(mock_reddit_tibble, n = 100)
  expect_false("the" %in% result_all$word)
  expect_false("is" %in% result_all$word)
})