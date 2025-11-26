options(
  pillar.max_char = 30,   
  tibble.width = Inf      
)

library(stringr)
library(tidyverse)
library(httr)
library(jsonlite)

#' Scrapes posts from a subreddit using pagination.
#'
#' This function calls the Reddit API in an iterative way (pagination) to surpass the 100-post limit 
#' per request and collects until the specified limit.
#'
#' @param subreddit a string that contains the subbredit name,
#'   the format 'r/name' or a complete Reddit URL. It is validated using \code{validate_subreddit}.
#' @param post_limit The maximum number of posts that are going to be scraped.
#'   (Defaut value: 500).
#'
#' @return A \code{tibble} that contains the columns \code{title} y \code{selftext}
#'   with the data from the posts. The number of rows is lower or equal than \code{post_limit}.
#'
#' @examples
#' \dontrun{
#' # Scrape the hottest first 250 posts from r/rstats
#' rstats_data <- scrape_subreddit("r/rstats", post_limit = 250)
#' head(rstats_data)
#' }
#' @export
scrape_subreddit <- function(subreddit, post_limit = 500) {
  # Call the existing function to extract and clean subreddit link
  subreddit_name <- validate_subreddit(subreddit)
  
  # API constraint
  PAGE_SIZE <- 100
  
  # Variables to store data and control the loop
  all_data <- tibble(title = character(), selftext = character())
  current_after <- NULL
  posts_scraped <- 0
  
  # Fetching total limit
  num_pages <- ceiling(min(post_limit, 1000) / PAGE_SIZE)
  
  cat("Fetching", post_limit, "posts in", num_pages, "pages...\n")
  
  for (i in 1:num_pages) {
    url <- paste0("https://old.reddit.com/r/", subreddit_name, "/hot/.json?limit=", PAGE_SIZE)
    
    if (!is.null(current_after)) {
      url <- paste0(url, "&after=", current_after)
    }
    
    # Fetch data
    response <- GET(url, user_agent("redditR/0.1 by Manuel_S"))
    
    # Handle response codes
    if (http_error(response)) {
      warning(paste("HTTP error on page", i, ":", status_code(response)))
      break
    }
    
    # Parse response
    parsed <- fromJSON(rawToChar(response$content))
    
    if (is.null(parsed$data$children)) {
      cat("Reached end of subreddit or unexpected structure on page", i, "\n")
      break
    }
    
    # Extract and combine data
    titles <- parsed$data$children$data$title
    self_texts <- parsed$data$children$data$selftext
    
    page_data <- tibble(title = titles, selftext = self_texts)
    
    all_data <- bind_rows(all_data, page_data)
    
    current_after <- parsed$data$after
    posts_scraped <- posts_scraped + nrow(page_data)
    
    cat("Page", i, "collected:", nrow(page_data), "posts. Total:", posts_scraped, "posts.\n")
    
    # Stop if we hit the total limit or the 'after' token is NULL
    if (is.null(current_after) || current_after == "" || posts_scraped >= post_limit) {
      break
    }
    
    # Pause to respect API rate limits
    Sys.sleep(1) 
  }
  
  return(head(all_data, post_limit))
}