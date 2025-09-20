# --- Prerequisites ---
# Ensure you have the necessary packages installed.
# If not, uncomment and run the following lines:
# install.packages("quantmod")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr") # Optional, can use base R alternatives
# install.packages("scales") # For formatting axis labels

# Load required libraries
library(quantmod)
library(ggplot2)
library(lubridate) # For easy date calculations
library(dplyr)     # For data manipulation (optional)
library(scales)    # For formatting plot axes

# --- User Specifications ---
# 1. Ticker Symbol (e.g., "DIA", "SPY", "AAPL")
ticker <- "DIA"

# 2. Number of days for return calculation (n)
n_days <- 5        # e.g., 1 for daily, 5 for weekly, 21 for ~monthly

# 3. Number of past years of data to fetch (m)
m_years <- 10

# 4. Use overlapping or non-overlapping returns?
#    Set to TRUE for overlapping (calculates return for every possible n-day period)
#    Set to FALSE for non-overlapping (calculates returns for distinct n-day periods)
use_overlapping <- TRUE
# --- End User Specifications ---

# --- Data Fetching and Preparation ---

# Calculate start and end dates for fetching data
end_date <- Sys.Date()
start_date <- end_date - years(m_years) # `years()` function from lubridate

# Fetch historical stock data using quantmod from Yahoo Finance
stock_data <- NULL # Initialize variable
cat("Fetching data for:", ticker, "from", start_date, "to", end_date, "\n")
tryCatch({
  stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  
  # Validate fetched data
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    stop("No data returned from Yahoo Finance. Check ticker symbol ('", ticker, "') and date range.")
  }
  
  # Select the Adjusted Close prices column
  prices <- Ad(stock_data)
  colnames(prices) <- "Adjusted_Close"
  
}, error = function(e) {
  stop(paste("Error fetching data for ticker '", ticker, "': ", e$message, sep = ""))
})

# Remove any potential NA values in the price series
prices <- na.omit(prices)
cat("Fetched", nrow(prices), "trading days of data.\n")

# Check if enough data exists for the calculation after removing NAs
if (nrow(prices) < n_days + 1) {
  stop(paste("Not enough data points (", nrow(prices), ") available for '", ticker,
             "' to calculate ", n_days, "-day returns over the specified period.", sep = ""))
}

# --- Return Calculation ---
cat("Calculating", n_days, "-day returns...\n")
returns_n_day <- Delt(prices$Adjusted_Close, k = n_days, type = "arithmetic")
returns_n_day <- na.omit(returns_n_day)
colnames(returns_n_day) <- "n_Day_Return"

# --- Handle Overlapping vs. Non-Overlapping ---
if (use_overlapping) {
  final_returns <- returns_n_day
  return_type_label <- "Overlapping"
  cat("Using overlapping returns.\n")
} else {
  indices_for_non_overlapping <- seq(from = n_days, to = length(returns_n_day), by = n_days)
  if (length(indices_for_non_overlapping) == 0) {
    stop(paste("Not enough data points (", nrow(returns_n_day), " overlapping returns)",
               " to calculate any non-overlapping ", n_days, "-day returns.", sep=""))
  }
  final_returns <- returns_n_day[indices_for_non_overlapping, , drop = FALSE]
  return_type_label <- "Non-Overlapping"
  cat("Using non-overlapping returns.\n")
}

if (is.null(final_returns) || nrow(final_returns) == 0) {
  stop("No return data available after processing. Check parameters or data source.")
}
cat("Calculated", nrow(final_returns), return_type_label, n_days, "-day returns.\n")

# --- Plotting ---

# Convert the final returns (xts object) to a data frame for ggplot2
returns_df <- data.frame(
  Date = index(final_returns),
  Return = coredata(final_returns)[,1]
)

# Calculate the mean return *before* plotting to use it in the plot layers
mean_ret <- mean(returns_df$Return, na.rm = TRUE) # Use na.rm for safety

# Define plot titles and labels
plot_title <- paste(ticker, ": Histogram of", n_days, "-Day", return_type_label, "Returns")
base_subtitle <- paste("Data Period:", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"))
# Include mean in the subtitle
plot_subtitle <- paste(base_subtitle, "\nMean Return:", percent(mean_ret, accuracy = 0.01))
x_label <- paste(n_days, "-Day Return", sep = "")
caption_label <- paste("Data Source: Yahoo Finance | Total Returns:", nrow(returns_df))

# Create the histogram using ggplot2
hist_plot <- ggplot(returns_df, aes(x = Return)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "steelblue",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  
  # --- Add vertical line for the mean ---
  geom_vline(aes(xintercept = mean_ret, # Set the position to the calculated mean
  ),
  color = "darkgreen",       # Choose a distinct color
  linetype = "dashed",     # Choose a line style (e.g., "dashed", "dotted")
  linewidth = 1) +            # Choose line thickness
  
  # Apply labels and title
  labs(
    title = plot_title,
    subtitle = plot_subtitle, # Subtitle now includes the mean value
    x = x_label,
    y = "Density",
    caption = caption_label
  ) +
  # Use a minimal theme
  theme_minimal() +
  # Format the x-axis labels as percentages
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Print the plot to the R graphics device
print(hist_plot)

# --- Optional: Display Summary Statistics ---
cat("\nSummary statistics for the calculated returns:\n")
summary_stats <- summary(returns_df$Return)
quantiles <- quantile(returns_df$Return, probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99), na.rm = TRUE)
# mean_ret is already calculated above
sd_ret <- sd(returns_df$Return, na.rm = TRUE)

print(summary_stats)
cat("\nPercentiles:\n")
print(percent(quantiles, accuracy=0.01))
cat(paste("\nMean:", percent(mean_ret, accuracy=0.01))) # Use the pre-calculated mean
cat(paste("\nStandard Deviation:", percent(sd_ret, accuracy=0.01)), "\n")
