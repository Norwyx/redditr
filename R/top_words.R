library(tidytext)
library(dplyr)
library(stringr)

#' Identifies the most frequent words in the Reddit posts.
#'
#' This function combines the titles and body of the posts, tokenizes the text,
#' removes 'stopwords' and short words, and calculates the N most used words.
#'
#' @param reddit_tibble A \code{tibble} that contains at least the columns
#'   \code{title} y \code{selftext} (eg. the result from \code{scrape_subreddit}).
#' @param n The number of the most frequent words to return. (Default value: 500).
#'
#' @return A \code{tibble} with the columns \code{word} (word) and \code{n} (freequency),
#'   sorted from most to least frequency.
#'
#' @examples
#' \dontrun{
#' # Assuming that 'data_scraped' is the result from scrape_subreddit
#' data_scraped <- tibble::tibble(
#'   title = c("R programming is great", "Python is better, test"), 
#'   selftext = c("I love R, I do", "Test, test, test.")
#' )
#' top_10_words <- top_words(data_scraped, n = 10)
#' print(top_10_words)
#' }
#' @export
top_words <- function(reddit_tibble, n = 500) {
  # Prepare and combine the 2 columns into one called "text" and replace all NA values for a space
  text_data <- reddit_tibble |>
    mutate(
      text = str_c(title, " ", selftext, sep = " "),
      text = ifelse(is.na(text) | str_trim(text) == "", "", text)
    ) |>
    select(text)
  
  # Tokenize the new tibble
  tokens <- text_data |>
    unnest_tokens(word, text)
  
  # Remove stop words, short words, and non-alphanumeric characters
  data("stop_words")
  
  tokens_filtered <- tokens |>
    anti_join(stop_words, by = "word") |>
    filter(str_detect(word, "^[a-z]{3,}$"))
  
  # Count words, sort them, and return the top N words
  word_counts <- tokens_filtered |>
    count(word, sort = TRUE)
  
  return(head(word_counts, n))
}