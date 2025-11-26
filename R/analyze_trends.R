library(ggplot2)

#' Analyzes the trending topics and the general sentiment of the posts.
#'
#' This function tokenizes, cleans the text, and then generates two plots:
#' a bar chart of the most frequent words (dominant topics) and
#' a bar chart of the general sentiment (positive vs. negative polarity).
#'
#' @param reddit_data A \code{tibble} that at least contains the columns
#'   \code{title} y \code{selftext} (eg. the result of \code{scrape_subreddit}).
#'
#' @return a list that contains two objects \code{ggplot}: \code{plot_temas}
#'   (frequency) and \code{plot_sentimiento} (polarity).
#'
#' @examples
#' \dontrun{
#' # Assuming that 'data_scraped' is the result from scrape_subreddit
#' # For this example, ggplot2 and tidytext are needed.
#' data_scraped <- tibble::tibble(
#'   title = c("I love this code", "This is terrible"), 
#'   selftext = c("Great success!", "Error and failure.")
#' )
#' results <- analyze_trends(data_scraped)
#' print(results$plot_sentimiento)
#' }
#' @export
analyze_trends <- function(reddit_data){
  # Reuse logic from "top_words" function to clean data
  text_data <- reddit_data |>
    mutate(
      text = str_c(title, " ", selftext, sep = " "),
      text = ifelse(is.na(text) | str_trim(text) == "", "", text)
    ) |>
    select(text)
  
  tokens <- text_data |>
    unnest_tokens(word, text)
  
  data("stop_words")
  
  tokens_filtered <- tokens |>
    anti_join(stop_words, by = "word") |>
    filter(str_detect(word, "^[a-z]{3,}$"))
  
  # Count the words, get the top 15 by frequency, and create the plot with those
  word_counts <- tokens_filtered |> 
    count(word, sort = TRUE) |>
    head(15)
  
  frequency_plot <- word_counts |>
    mutate(word = reorder(word, n)) |>
    ggplot(aes(x = word, y = n)) +
    geom_col(fill = "skyblue") +
    coord_flip() + 
    labs(
      title = "Main Topics (Top 15 Words)",
      x = "Word",
      y = "Frequency"
    ) +
    theme_minimal()
  
  # Rank words by sentiment, count polarity, and plot it
  
  sentiments <- tokens_filtered |>
    inner_join(get_sentiments("bing"), by = "word")
  
  sentiment_count <- sentiments |>
    count(sentiment, sort = TRUE)
  
  sentiment_plot <- sentiment_count |>
    ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
    geom_col() +
    scale_fill_manual(values = c("negative" = "#F8766D", "positive" = "#00BFC4")) +
    labs(
      title = "General Sentiment of the Content",
      x = "Polarity",
      y = "Number of Classified Words"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(list(
    frequency_plot = frequency_plot,
    sentiment_plot = sentiment_plot
    ) )
}