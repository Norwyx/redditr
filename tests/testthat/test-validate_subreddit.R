library(testthat)
library(stringr)
library(httr)
library(jsonlite)

test_that("validate_subreddit cleans and returns the name for valid inputs", {
  skip_on_cran() 
  
  expect_equal(validate_subreddit("Colombia"), "Colombia") 
  
  expect_equal(validate_subreddit("r/Colombia"), "Colombia")
  
  expect_equal(validate_subreddit("https://old.reddit.com/r/Colombia/"), "Colombia")
  
  expect_equal(validate_subreddit("R_programming"), "R_programming")
})

 
test_that("validate_subreddit generates an error for non-existent or invalid subreddits", {
  skip_on_cran()
    
  expect_error(
    validate_subreddit("SubThatDoesNotExistAAAAA"),
    regexp = "404|403|not found|private",    
    ignore.case = TRUE
  )
    
  expect_error(
    validate_subreddit("reddit.com/r/"),
    regexp = "Invalid",
    ignore.case = TRUE
  )
    
  expect_error(
    validate_subreddit(""),
    regexp = "Invalid",
    ignore.case = TRUE
  )
})