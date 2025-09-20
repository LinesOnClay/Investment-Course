
# --- Generate QQ Plot ---
# This section assumes the script above has been run,
# and the 'returns_df', 'ticker', 'n_days', 'return_type_label',
# 'start_date', and 'end_date' variables exist.

cat("\n--- Generating Normal Q-Q Plot ---\n")

# Check if the necessary data frame exists
if (!exists("returns_df") || !is.data.frame(returns_df) || nrow(returns_df) == 0) {
    stop("Return data frame 'returns_df' not found or is empty. Please run the data calculation part first.")
}

# Create the QQ plot using ggplot2
qq_plot <- ggplot(returns_df, aes(sample = Return)) + # Map the 'Return' column to the 'sample' aesthetic
  stat_qq(color = "steelblue", alpha = 0.7) +        # Plot the quantile points
  stat_qq_line(color = "red",                       # Add the theoretical quantile line
               linetype = "dashed",
               linewidth = 0.8) +
  labs(
    title = paste(ticker, ": Normal Q-Q Plot of", n_days, "-Day", return_type_label, "Returns"),
    subtitle = paste("Data Period:", format(start_date, "%Y-%m-%d"), "to", format(Sys.Date(), "%Y-%m-%d")), # Use Sys.Date() as end date was defined as such
    x = "Theoretical Quantiles (Standard Normal)", # Label for x-axis
    y = "Sample Quantiles (Actual Returns)",       # Label for y-axis
    caption = paste("Data Source: Yahoo Finance | N =", nrow(returns_df)) # Add sample size
  ) +
  theme_minimal(base_size = 11) + # Use a minimal theme
  theme(
      plot.title = element_text(hjust = 0.5, size=rel(1.2)), # Center title, adjust size
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 1, size=rel(0.9), color="grey40") # Adjust caption
  )

# Print the QQ plot to the current graphics device
print(qq_plot)

cat("Normal Q-Q plot displayed.\n")
