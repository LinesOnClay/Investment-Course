# --- Prerequisites ---
# Ensure you have the necessary packages installed.
# If not, uncomment and run the following lines:
# install.packages("quantmod")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr") # Optional, can use base R alternatives
# install.packages("scales") # For formatting axis labels
# install.packages("gt")     # For creating attractive tables

# Load required libraries
library(quantmod)
library(ggplot2)
library(lubridate) # For easy date calculations
library(dplyr)     # For data manipulation (optional)
library(scales)    # For formatting plot axes
library(gt)        # Load the gt package

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
end_date <- Sys.Date()
start_date <- end_date - years(m_years)
stock_data <- NULL
cat("Fetching data for:", ticker, "from", start_date, "to", end_date, "\n")
tryCatch({
  stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    stop("No data returned from Yahoo Finance. Check ticker symbol ('", ticker, "') and date range.")
  }
  prices <- Ad(stock_data)
  colnames(prices) <- "Adjusted_Close"
}, error = function(e) {
  stop(paste("Error fetching data for ticker '", ticker, "': ", e$message, sep = ""))
})
prices <- na.omit(prices)
cat("Fetched", nrow(prices), "trading days of data.\n")
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
returns_df <- data.frame(
  Date = index(final_returns),
  Return = coredata(final_returns)[,1]
)
mean_ret <- mean(returns_df$Return, na.rm = TRUE)
plot_title <- paste(ticker, ": Histogram of", n_days, "-Day", return_type_label, "Returns")
base_subtitle <- paste("Data Period:", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"))
plot_subtitle <- paste(base_subtitle, "\nMean Return:", percent(mean_ret, accuracy = 0.01))
x_label <- paste(n_days, "-Day Return", sep = "")
caption_label <- paste("Data Source: Yahoo Finance | Total Returns:", nrow(returns_df))

hist_plot <- ggplot(returns_df, aes(x = Return)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = mean_ret), color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(title = plot_title, subtitle = plot_subtitle, x = x_label, y = "Density", caption = caption_label) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(hist_plot) # Display the plot

# --- Create Attractive Summary Table using gt ---
cat("\n--- Summary Statistics ---\n")

# Calculate necessary statistics
n_obs <- nrow(returns_df)
sd_ret <- sd(returns_df$Return, na.rm = TRUE)
# Get standard summary values (Min, Q1, Median, Mean, Q3, Max)
std_summary_vals <- summary(returns_df$Return)
# Get specific quantiles
spec_quantiles <- quantile(returns_df$Return, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE)

# Create a data frame structured for the gt table
# Using names helps if the order of summary() output changes, but indexing (used below) is safer
summary_df <- data.frame(
  Statistic = factor(c("Observations (N)", "Minimum (0%)", "1st Percentile", "5th Percentile",
                       "1st Quartile (25%)", "Median (50%)", "Mean", "3rd Quartile (75%)",
                       "95th Percentile", "99th Percentile", "Maximum (100%)", "Standard Deviation"),
                     # Set levels for desired display order
                     levels = c("Observations (N)", "Minimum (0%)", "1st Percentile", "5th Percentile",
                                "1st Quartile (25%)", "Median (50%)", "Mean", "3rd Quartile (75%)",
                                "95th Percentile", "99th Percentile", "Maximum (100%)", "Standard Deviation")),
  # Use indexing for safety in case summary() output changes format
  Value = c(as.numeric(n_obs),             # N
            as.numeric(std_summary_vals[1]), # Min
            as.numeric(spec_quantiles["1%"]),
            as.numeric(spec_quantiles["5%"]),
            as.numeric(std_summary_vals[2]), # Q1
            as.numeric(std_summary_vals[3]), # Median
            as.numeric(std_summary_vals[4]), # Mean (same as mean_ret)
            as.numeric(std_summary_vals[5]), # Q3
            as.numeric(spec_quantiles["95%"]),
            as.numeric(spec_quantiles["99%"]),
            as.numeric(std_summary_vals[6]), # Max
            as.numeric(sd_ret))             # Std Dev
)

# Create and style the gt table
summary_table <- gt(summary_df) %>%
  tab_header(
    title = md(paste("**Summary Statistics:**", n_days, "-Day Returns")), # Use markdown for bold
    subtitle = md(paste0("*", ticker, "* | ", return_type_label, " | ",
                         format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d"))) # Use markdown for italics
  ) %>%
  # Format the 'Value' column based on the 'Statistic'
  fmt_number(
    columns = Value,
    rows = Statistic == "Observations (N)", # Format count as integer
    decimals = 0,
    use_seps = TRUE # Use thousands separators like comma
  ) %>%
  fmt_percent(
    columns = Value,
    rows = Statistic != "Observations (N)", # Format all others as percentages
    decimals = 2 # Use 2 decimal places for percentages
  ) %>%
  # Improve column labels
  cols_label(
    Statistic = "Metric",
    Value = "Value"
  ) %>%
  # Add some styling
  opt_all_caps(locations = "column_labels") %>% # Uppercase column labels
  tab_style( # Align Metric column left
    style = cell_text(align = "left"),
    locations = cells_body(columns = Statistic)
  ) %>%
  tab_style( # Align Value column right
    style = cell_text(align = "right"),
    locations = cells_body(columns = Value)
  ) %>%
  # Add a source note
  tab_source_note(
    source_note = md("*Returns calculated from Yahoo Finance Adjusted Close prices.*") # Markdown italics
  ) %>%
  # Add overall table options
  tab_options(
    table.border.top.color = "grey",
    heading.border.bottom.color = "grey",
    column_labels.border.bottom.color = "grey",
    table_body.border.bottom.color = "grey",
    table.width = pct(70) # Adjust width %
  )

# Print the gt table (will render nicely in RStudio viewer or RMarkdown output)
print(summary_table)


cat("\n--- End of Analysis ---\n")

