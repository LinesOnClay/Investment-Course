# Load necessary libraries
library(ggplot2)
library(quantmod) # For fetching financial data
library(stringr) # For string manipulation
library(lubridate) # For date manipulation

# ====================================================================
# USER INPUTS: Modify these variables directly in the script below
# ====================================================================

# 1. Enter stock tickers (comma-separated within the c() function, up to 12)
#    Example: c("KO", "VZ", "DIS", "AAPL", "MSFT")
user_tickers <- c("GE", "NVDA", "XLP", "XLK", "EFA", "AGG", "TSLA", "PFE")
# user_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "XLF", "SPY") # Uncomment and modify for other stocks

# 2. Define the Start Date for the price performance (YYYY-MM-DD)
start_date <- as.Date("2024-04-09")
# start_date <- as.Date("2023-01-01") # Uncomment and modify for a different start date

# 3. Define the End Date for the price performance (YYYY-MM-DD)
end_date <- as.Date("2025-09-20")
# end_date <- as.Date("2024-12-31") # Uncomment and modify for a different end date

# ====================================================================
# END OF USER INPUTS - Do not modify code below unless you know what you are doing
# ====================================================================

# Limit to 12 tickers if more are provided (for plot readability)
if (length(user_tickers) > 12) {
  user_tickers <- user_tickers[1:12]
  message("Limiting to the first 12 tickers provided in 'user_tickers'.")
}

# --- Data Fetching Functions ---

# Function to get full company or fund name from ticker
# This is a simplified lookup. For a truly robust solution, a financial data API is needed.
get_company_name <- function(ticker) {
  switch(toupper(ticker),
         "KO" = "Coca-Cola Co.",
         "VZ" = "Verizon Communications Inc.",
         "DIS" = "Walt Disney Co.",
         "META" = "Meta Platforms Inc. (Class A)",
         "CAT" = "Caterpillar Inc.",
         "CRM" = "Salesforce Inc.",
         "TSLA" = "Tesla, Inc.",
         "PFE" = "Pfizer Inc.",
         "GE" = "General Electric Co.",
         "NVDA" = "NVIDIA Corp.",
         "XLP" = "Consumer Staples Select Sector SPDR Fund",
         "XLK" = "Technology Select Sector SPDR Fund",
         "EFA" = "iShares MSCI EAFE ETF",
         "AGG" = "iShares Core U.S. Aggregate Bond ETF",
         "AAPL" = "Apple Inc.",
         "GOOGL" = "Alphabet Inc. (Class A)",
         "MSFT" = "Microsoft Corp.",
         "AMZN" = "Amazon.com Inc.",
         ticker # Return ticker if name not found in this simple list
  )
}

# Function to get price return over a specified period
get_return_for_period <- function(ticker, start_date, end_date) {
  tryCatch({
    # Use the simple Close price for a price-only return
    stock_data <- getSymbols(ticker, src = "yahoo",
                             from = start_date, to = end_date,
                             auto.assign = FALSE)
    
    if (is.null(stock_data) || NROW(stock_data) < 2) {
      warning(paste("Could not retrieve enough data for", ticker, "between", start_date, "and", end_date))
      return(NA)
    }
    
    # Use the Close price (Cl) for price-only return calculation
    closing_prices <- Cl(stock_data)
    
    if (NROW(closing_prices) < 2) {
      warning(paste("Not enough closing price data for", ticker, "to calculate return."))
      return(NA)
    }
    
    # Get the first and last closing price in the period
    start_price <- as.numeric(closing_prices[1])
    end_price <- as.numeric(closing_prices[NROW(closing_prices)])
    
    if (is.na(start_price) || is.na(end_price) || start_price == 0) {
      warning(paste("Invalid start or end price for", ticker, ". Cannot calculate return."))
      return(NA)
    }
    
    # Calculate arithmetic return as a percentage
    return(((end_price - start_price) / start_price) * 100)
  }, error = function(e) {
    warning(paste("Error fetching data for", ticker, ":", e$message))
    return(NA)
  })
}

# --- Data Collection ---
company_data_list <- list()

# Check and reorder dates if necessary
if (start_date > end_date) {
  message("Start date is after end date. Swapping dates for calculation.")
  temp_date <- start_date
  start_date <- end_date
  end_date <- temp_date
}

for (ticker in user_tickers) {
  company_name <- get_company_name(ticker)
  return_val <- get_return_for_period(ticker, start_date, end_date)
  
  if (!is.na(return_val)) {
    company_data_list[[ticker]] <- data.frame(
      company_ticker = ticker,
      company = company_name,
      return_value = return_val,
      stringsAsFactors = FALSE
    )
  }
}

if (length(company_data_list) == 0) {
  stop("No valid stock data could be retrieved for the given tickers and dates. Please check tickers and dates.")
}

company_data <- do.call(rbind, company_data_list)

# --- Data Preparation for Plotting ---

# Create a category for gain or loss based on return
company_data$performance_category <- ifelse(company_data$return_value >= 0, "Gain", "Loss")
company_data$performance_category <- factor(company_data$performance_category, levels = c("Loss", "Gain")) # Ensure order for color mapping

# Color scheme for gain/loss
colors <- c("Loss" = "#DC5318", "Gain" = "#333f48")

# --- Create the Cleveland Dot Plot ---

ggplot(company_data, aes(x = return_value, y = reorder(company, return_value))) +
  geom_segment(aes(xend = 0, yend = company), color = "grey50") + # Line from 0 to the point
  geom_point(aes(color = performance_category), size = 4) + # The dot, colored by gain/loss
  scale_color_manual(values = colors) + # Apply custom colors
  scale_x_continuous(labels = function(x) paste0(x, "%")) + # Format x-axis as percentages
  geom_text(aes(label = paste0(round(return_value, 1), "%")), # Add percentage labels next to points
            hjust = 0.5, vjust = 2.0, size = 3) + # Adjust label position
  labs(title = paste0("Price Performance by Company (", format(start_date, "%b %d, %Y"), " - ", format(end_date, "%b %d, %Y"), ")"),
       x = "Price Return",
       y = "Company") +
  theme_minimal() + # Use a minimalist theme
  theme(legend.position = "bottom") # Place the legend at the bottom
