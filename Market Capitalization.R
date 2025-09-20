library(quantmod)
library(ggplot2)


# Define your list of company ticker symbols (up to 7)
# Common tickers: "AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "TSLA", "META"
# Other examples for you to try: "JPM", "V", "LLY", "XOM", "M"
my_tickers <- c( "JPM", "KO", "LLY", "BABA", "AMZN", "GM",
                 "NVDA")
# my_tickers <- c("LLY", "JPM", "V")
# my_tickers <- c("F", "GM", "TM") # Ford, General Motors, Toyota
# my_tickers <- c("NONEXISTENTTICKER", "AAPL") # Example with a bad ticker






# --- Function to Fetch Market Cap and Company Name ---
get_market_cap_data <- function(ticker) {
  Sys.sleep(0.2) # Small delay to be polite to the server
  data <- NULL
  tryCatch({
    # Fetch Market Capitalization and Company Name
    quote_data <- quantmod::getQuote(
      Symbols = ticker,
      src = "yahoo",
      what = yahooQF(c("Market Capitalization", "Name"))
    )
    
    # Check if data was returned and has the expected columns
    if (!is.null(quote_data) &&
        "Market Capitalization" %in% colnames(quote_data) &&
        "Name" %in% colnames(quote_data)) {
      
      market_cap <- quote_data[, "Market Capitalization"]
      company_name <- quote_data[, "Name"]
      
      # Ensure market_cap is numeric
      if (is.character(market_cap)) {
        market_cap <- as.numeric(gsub(",", "", market_cap)) # Remove commas if any
      }
      
      if (!is.na(market_cap) && !is.na(company_name)) {
        data <- data.frame(
          Ticker = ticker,
          CompanyName = company_name,
          MarketCap = as.numeric(market_cap),
          stringsAsFactors = FALSE
        )
      }
    }
  }, error = function(e) {
    warning(paste("Could not retrieve data for ticker:", ticker, "-", e$message))
    data <- NULL
  })
  
  if (is.null(data) || nrow(data) == 0 || is.na(data$MarketCap)) {
    return(data.frame(
      Ticker = ticker,
      CompanyName = ticker, # Fallback to ticker if name/cap not found
      MarketCap = NA,
      stringsAsFactors = FALSE
    ))
  }
  return(data)
}


# --- Main Function to Create Plot ---
plot_market_caps <- function(ticker_symbols) {
  if (length(ticker_symbols) == 0) {
    stop("Please provide at least one ticker symbol.")
  }
  if (length(ticker_symbols) > 7) {
    stop("Please provide no more than 7 ticker symbols.")
  }
  
  # Fetch data for all tickers
  market_data_list <- lapply(ticker_symbols, get_market_cap_data)
  market_df <- do.call(rbind, market_data_list)
  
  # Remove any companies for which market cap data couldn't be found
  market_df <- market_df[!is.na(market_df$MarketCap) & market_df$MarketCap > 0, ]
  
  if (nrow(market_df) == 0) {
    message("Could not retrieve valid market cap data for any of the provided tickers.")
    return(invisible(NULL)) # Return nothing if no data
  }
  
  # Format MarketCap for better readability on the plot
  # (e.g., $2.50T, $500.00B, $10.00M)
  format_cap_labels <- function(mcaps) {
    sapply(mcaps, function(mcap) {
      if (is.na(mcap)) return(NA_character_)
      abs_mcap <- abs(mcap) # Use absolute for magnitude calculation
      sign_mcap <- ifelse(mcap < 0, "-", "")
      
      if (abs_mcap >= 1e12) {
        val <- paste0(sign_mcap, "$", format(round(abs_mcap / 1e12, 2), nsmall = 2), "T")
      } else if (abs_mcap >= 1e9) {
        val <- paste0(sign_mcap, "$", format(round(abs_mcap / 1e9, 2), nsmall = 2), "B")
      } else if (abs_mcap >= 1e6) {
        val <- paste0(sign_mcap, "$", format(round(abs_mcap / 1e6, 2), nsmall = 2), "M")
      } else if (abs_mcap >= 1e3) {
        val <- paste0(sign_mcap, "$", format(round(abs_mcap / 1e3, 2), nsmall = 2), "K")
      } else {
        val <- paste0(sign_mcap, "$", format(round(abs_mcap, 0), nsmall = 0))
      }
      return(val)
    })
  }
  
  # Define colors
  background_color <- "#f5f7fa"
  label_color <- "#2a6ca3"
  bar_color <- "#e8c547"
  
  # Create the plot
  # Using reorder(CompanyName, MarketCap) orders the y-axis by MarketCap in ascending order
  p <- ggplot(market_df, aes(x = MarketCap, y = reorder(CompanyName, MarketCap))) +
    geom_bar(stat = "identity", fill = bar_color, width = 0.7) +
    geom_text(
      aes(label = format_cap_labels(MarketCap)),
      hjust = -0.1,
      size = 3.5,
      color = label_color
    ) +
    scale_x_continuous(
      labels = function(x) format_cap_labels(x),
      expand = expansion(mult = c(0.01, 0.18)) # Expand x-axis to fit text labels
    ) +
    labs(
      title = "Company Market Capitalizations",
      subtitle = paste("Data fetched on:", format(Sys.Date(), "%B %d, %Y")),
      x = "Market Cap (USD)",
      y = "Company"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = background_color, color = background_color),
      panel.background = element_rect(fill = background_color, color = background_color),
      panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(), # No horizontal grid lines for horizontal bars
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(color = label_color, margin = margin(t = 10), face="bold"),
      axis.title.y = element_text(color = label_color, margin = margin(r = 10), face="bold"),
      axis.text.x = element_text(color = label_color, angle = 0, hjust = 1),
      axis.text.y = element_text(color = label_color, hjust = 1), # Ensure y-axis labels are right-aligned
      plot.title = element_text(color = label_color, hjust = 0.5, size = 16, face = "bold", margin = margin(b=5)),
      plot.subtitle = element_text(color = label_color, hjust = 0.5, size = 10, margin = margin(b=15)),
      legend.position = "none"
    )
  
  return(p)
}

# --- Example Usage ---
# Define your list of company ticker symbols (up to 7)
# Common tickers: "AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "TSLA", "META"
# Other examples: "JPM", "V", "LLY", "XOM"
#my_tickers <- c("HD", "DIS", "GE", "LLY", "BABA", "AMZN", "YUM")
# my_tickers <- c("LLY", "JPM", "V")
# my_tickers <- c("F", "GM", "TM") # Ford, General Motors, Toyota
# my_tickers <- c("NONEXISTENTTICKER", "AAPL") # Example with a bad ticker

# Generate and display the plot
# If running in RStudio, the plot will appear in the Plots pane.
if(length(my_tickers) > 0 && length(my_tickers) <= 7) {
  market_cap_plot <- plot_market_caps(my_tickers)
  if (!is.null(market_cap_plot)) {
    print(market_cap_plot)
    
    # Optional: Save the plot
    # ggsave("market_cap_chart.png", plot = market_cap_plot, width = 10, height = 6, dpi = 300, bg = "#f5f7fa")
    # message("Plot saved as market_cap_chart.png")
  }
} else if (length(my_tickers) > 7) {
  message("Too many ticker symbols. Please provide up to 7.")
} else {
  message("No ticker symbols provided.")
}


