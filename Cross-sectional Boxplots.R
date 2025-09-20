# --- 0. Load Libraries ---
library(quantmod) # For getSymbols
library(TTR)      # For ROC
library(dplyr)    # For %>% and data manipulation
library(ggplot2)  # For plotting
library(scales)   # For percent_format

# --- 1. Define Color Palette ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
PALETTE_BOXPLOT <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318")

# --- 2. Parameters ---
tickers <- c("XLE", "XLK", "XLY", "XLP", "XLV") # Enter company tickers
n_day <-21   # Enter the return period (e.g., 1 for daily, 5 for weekly, 21 for monthly approx)
m_years <- 10 # Enter the number of years for the lookback period

# --- 3. Calculate Date Range ---
end_date <- Sys.Date()
start_date <- end_date - years(m_years) # 'years' function from lubridate might be needed if not base R

cat(paste("Fetching data from", start_date, "to", end_date, "\n"))

# --- 4. Download and Process Data ---
all_returns_list <- list() # Initialize list to store results

for (ticker in tickers) {
  cat(paste("Processing:", ticker, "... "))
  stock_data_xts <- tryCatch({
    getSymbols(ticker,
               src = "yahoo",
               from = start_date,
               to = end_date,
               auto.assign = FALSE) # Get data directly into variable
  }, error = function(e) {
    cat(paste(" FAILED to download data for", ticker, "\n"))
    return(NULL)
  })
  
  # Proceed only if data was fetched successfully
  if (!is.null(stock_data_xts) && nrow(stock_data_xts) > 0) {
    
    # Select Adjusted Close prices
    adj_close_prices <- Ad(stock_data_xts)
    
    # Calculate n-day returns (Rate of Change - discrete type is (price_t / price_{t-n}) - 1)
    n_day_returns_xts <- TTR::ROC(adj_close_prices, n = n_day, type = "discrete")
    
    # Convert to data frame
    returns_df <- data.frame(
      Date = index(n_day_returns_xts),
      Return = coredata(n_day_returns_xts)[,1] # Ensure it's a vector
    ) %>%
      mutate(Ticker = ticker) # Add ticker column
    
    # Add to the list
    all_returns_list[[ticker]] <- returns_df
    cat(" SUCCESS\n")
    
  } else {
    if (is.null(stock_data_xts)) {
      # Error message already printed by tryCatch
    } else {
      cat(paste(" SKIPPED - No data returned for", ticker, "in the period.\n"))
    }
  }
  Sys.sleep(0.5) # Small pause to avoid overwhelming Yahoo Finance
} # End of loop

# --- 5. Combine Data ---
if (length(all_returns_list) > 0) {
  combined_returns_df <- bind_rows(all_returns_list)
  
  # --- 6. Clean Data ---
  combined_returns_df <- combined_returns_df %>%
    na.omit() # Remove NAs from return calculations
  
  # --- 7. Create ggplot ---
  if (nrow(combined_returns_df) > 0) {
    return_boxplot <- ggplot(combined_returns_df, aes(x = Ticker, y = Return, fill = Ticker)) +
      geom_boxplot(
        show.legend = FALSE,
        outlier.shape = 21,
        outlier.size = 1.5,
        outlier.alpha = 0.5,
        # Set box outline color directly
        color = COLOR_AXIS_LINES_TITLE # This sets the border color of the boxes
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = paste(n_day, "-Day Return (%)")) +
      scale_fill_manual(values = PALETTE_BOXPLOT) + # Use your custom palette for fill
      labs(
        title = paste("Distribution of", n_day, "-Day Returns"),
        subtitle = paste("Past", m_years, "Years (", start_date, "to", end_date, ")"),
        x = "Company Ticker",
        caption = "Data Source: Yahoo Finance via quantmod"
      ) +
      theme_minimal(base_size = 12) + # Start with a minimal theme
      theme(
        # Plot background
        plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
        panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
        
        # Titles
        plot.title = element_text(hjust = 0.5, face = "bold", color = COLOR_AXIS_LINES_TITLE),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = COLOR_AXIS_FONT),
        
        # Axis text and lines
        axis.text.x = element_text(angle = 45, hjust = 1, color = COLOR_AXIS_FONT),
        axis.text.y = element_text(color = COLOR_AXIS_FONT),
        axis.title.x = element_text(color = COLOR_AXIS_FONT),
        axis.title.y = element_text(color = COLOR_AXIS_FONT),
        
        # Axis lines (tick marks and line along axis)
        axis.line = element_line(color = COLOR_AXIS_LINES_TITLE),
        axis.ticks = element_line(color = COLOR_AXIS_LINES_TITLE),
        
        # Panel grid lines
        panel.grid.major = element_line(color = "#e0e0e0", linetype = "dotted"), # Light grey for subtle grid
        panel.grid.minor = element_blank(), # No minor grid lines
        
        # Caption
        plot.caption = element_text(hjust = 1, size = 8, color = COLOR_AXIS_FONT)
      )
    
    # --- 8. Print Plot ---
    print(return_boxplot)
    
  } else {
    message("No valid return data available to plot after processing all tickers.")
  }
  
} else {
  message("Failed to process data for any tickers. Cannot generate plot.")
}