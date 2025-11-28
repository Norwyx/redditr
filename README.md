# redditr: Reddit Data Scraper & Analyzer 📦

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)

`redditr` is an R package designed to simplify the process of extracting data from Reddit, analyzing trending topics, and visualizing sentiment. It overcomes the standard API limitation of 100 posts by implementing pagination logic.

## Features

* **`scrape_subreddit()`**: Scrape thousands of posts using pagination (bypassing the 100-post limit).
* **`top_words()`**: Automatically clean, tokenize, and extract the most frequent words from posts.
* **`analyze_trends()`**: Generate ready-to-use `ggplot2` visualizations for topic frequency and sentiment analysis.
* **`validate_subreddit()`**: Robust validation to ensure subreddits exist and are accessible before scraping.

## Installation

You can install the development version of `redditr` directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Norwyx/redditr")
```

## Usage Example

Here is a quick example of how to analyze the r/rstats subreddit:
```
library(redditr)
library(ggplot2)

# 1. Scrape the last 500 posts from r/rstats
data <- scrape_subreddit("r/rstats", post_limit = 500)

# 2. Get the top 10 most used words
words <- top_words(data, n = 10)
print(words)

# 3. Analyze trends and sentiment
plots <- analyze_trends(data)

# View the frequency plot
print(plots$frequency_plot)

# View the sentiment plot
print(plots$sentiment_plot)
```

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## License

This project is licensed under the MIT License.
