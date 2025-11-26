library(tidyverse)
library(stringr)
library(httr)
library(jsonlite)

#' Validates and extracts the name of a subreddit.
#'
#' This function verifies if the input is a valid URL,'r/name' or just 'name'.
#' Also calls the reddit API to verify that the subreddit exists.
#'
#' @param url_or_name A string that contains the name of the subreddit,
#'   the format'r/name' or a complete Reddit URL.
#'
#' @return The clean name of the subreddit (just the name), if it is valid and if it exists.
#'   Stops the execcution with an error if the subdreddit does not exist or if it is invalid.
#'
#' @examples
#' \dontrun{
#' validate_subreddit("rprogramming")
#' validate_subreddit("https://old.reddit.com/r/dataisbeautiful/")
#' }
#' @export
validate_subreddit <- function(url_or_name) {
  pattern <- "^(https?://)?(www\\.)?(old\\.)?reddit\\.com/r/[A-Za-z0-9_]+/?$"
  simple_pattern <- "^r/[A-Za-z0-9_]+$"
  name_pattern <- "^[A-Za-z0-9_]+$"
  
  # Validate if name or link is valid
  if(!str_detect(url_or_name, pattern) &&
     !str_detect(url_or_name, simple_pattern) &&
     !str_detect(url_or_name, name_pattern)) {
    stop("Invalid URL or subreddit name.")
  }
  
  # Extract name from the input
  if (str_detect(url_or_name, "reddit\\.com/r/")) {
    subreddit_name <- str_extract(url_or_name, "(?<=reddit\\.com/r/)[A-Za-z0-9_]+")
  } else if (str_detect(url_or_name, "^r/")) {
    subreddit_name <- str_remove(url_or_name, "^r/")
  } else {
    subreddit_name <- url_or_name
  }
  
  # Check if subreddit exists
  url <- paste0("https://old.reddit.com/r/", subreddit_name, "/.json")
  response <- GET(url, user_agent("redditR/0.1 by Manuel_S"))
  
  # Verify HTTP status code
  if (http_error(response)) {
    status <- status_code(response)
    if (status == 404) {
      error_msg <- paste0("Subreddit '", subreddit_name, "' not found (HTTP 404).")
    } else if (status == 403) {
      error_msg <- paste0("Subreddit '", subreddit_name, "' is private or restricted (HTTP 403).")
    } else {
      error_msg <- paste0("Subreddit '", subreddit_name, "' resulted in HTTP error ", status, ".")
    }
    stop(error_msg)
  }
  
  # Try parsing JSON safely
  parsed <- tryCatch(
    fromJSON(rawToChar(response$content)),
    error = function(e) NULL
  )
  
  # Validation logic
  if (is.null(parsed)) {
    stop(paste0("Subreddit '", subreddit_name, "' does not exist (HTML returned)."))
  }
  
  if (!"data" %in% names(parsed) ||
      !"children" %in% names(parsed$data) ||
      length(parsed$data$children) == 0) {
    stop(paste0("Subreddit '", subreddit_name, "' does not exist, it is private, or it does not have any posts."))
  }
  
  return(subreddit_name)
}