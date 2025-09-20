library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# YOUR JOB: Enter tickers here! Keep the format 'Ticker', 'Ticker', 'Ticker'... 
tickers <- c("EFA","EEM","SPY", "IWM") 

# Calculate the date range for the last 12 months
end_date <- Sys.Date()
start_date <- end_date - years(1)

# Function to get financial data and calculate returns
get_returns <- function(ticker, start_date, end_date) {
  tryCatch({
    # Get historical prices
    prices <- getSymbols(ticker, src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)
    prices_df <- as.data.frame(prices)
    colnames(prices_df) <- gsub(paste0(ticker, '\\.'), '', colnames(prices_df))
    prices_df$Date <- as.Date(rownames(prices_df))
    prices_df <- prices_df %>% arrange(Date)
    
    # Get historical dividends
    dividends <- getDividends(ticker, from = start_date, to = end_date, auto.assign = FALSE)
    dividends_df <- as.data.frame(dividends)
    if (nrow(dividends_df) > 0) {
      colnames(dividends_df) <- c('Dividend')
      dividends_df$Date <- as.Date(rownames(dividends_df))
    } else {
      dividends_df <- data.frame(Date = as.Date(character()), Dividend = numeric())
    }
    
    # Ensure there are enough data points for calculations
    if (nrow(prices_df) < 2) {
      warning(paste('Not enough price data for', ticker))
      return(NULL)
    }
    
    # Calculate returns
    first_price <- prices_df$Adjusted[1]
    last_price <- prices_df$Adjusted[nrow(prices_df)]
    
    price_gain_loss <- (last_price - first_price) / first_price
    total_dividends <- sum(dividends_df$Dividend, na.rm = TRUE)
    dividend_income <- total_dividends / first_price
    
    data.frame(
      Ticker = ticker,
      Return_Type = c('Dividend_Income', 'Price_Gain_Loss'),
      Return = c(dividend_income, price_gain_loss)
    )
  }, error = function(e) {
    warning(paste('Could not retrieve data for', ticker, ':', e$message))
    return(NULL)
  })
}

# Apply the function to all tickers and combine results
all_returns <- bind_rows(lapply(tickers, get_returns, start_date = start_date, end_date = end_date))

if (is.null(all_returns) || nrow(all_returns) == 0) {
  stop('No data available to plot. Please check ticker symbols or date range.')
}

# Define custom colors
bg_color <- '#f5f7fa'
axis_text_color <- '#DC5318'
title_color <- '#2a6ca3'
dividend_color <- '#333f48'
price_color <- '#e8c547'

# Create the stacked bar chart
ggplot(all_returns, aes(x = Ticker, y = Return, fill = Return_Type)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(values = c('Dividend_Income' = dividend_color, 'Price_Gain_Loss' = price_color),
                    labels = c('Dividend/Interest Income', 'Price Gain/Loss')) +
  labs(title = 'Investment Performance: Dividend/Interest vs. Price Change (Last 12 Months)',
       y = 'Percentage Return',
       x = 'Ticker Symbol',
       fill = 'Return Type') +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    axis.text = element_text(color = axis_text_color),
    axis.title = element_text(color = axis_text_color),
    plot.title = element_text(color = title_color, hjust = 0.5, face = 'bold'),
    legend.text = element_text(color = axis_text_color),
    legend.title = element_text(color = axis_text_color),
    legend.background = element_rect(fill = bg_color, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave('stacked_bar_chart.png', width = 10, height = 6, bg = bg_color)