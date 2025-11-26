library(testthat)
library(tibble)
library(stringr)
library(httr)
library(jsonlite)

test_that("scrape_subreddit returns a tibble with the right amount of columns", {
  skip_on_cran() 
  
  result <- scrape_subreddit("r/test", post_limit = 2)
  
  expect_true(is_tibble(result))
  expect_equal(colnames(result), c("title", "selftext"))
  expect_true(nrow(result) > 0)
})
  

test_that("scrape_subreddit respects 'post limit", {
  skip_on_cran() 
    
  limit <- 3
  result <- scrape_subreddit("r/rprogramming", post_limit = limit)
    
  expect_equal(nrow(result), limit)
})