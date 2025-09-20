# Install and load necessary packages
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
if (!requireNamespace("TTR", quietly = TRUE)) {
  install.packages("TTR")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("lubridate", quietly = TRUE)) { # Added lubridate for 'years' function
  install.packages("lubridate")
}
if (!requireNamespace("stringr", quietly = TRUE)) { # Added stringr for str_to_title
  install.packages("stringr")
}
if (!requireNamespace("RColorBrewer", quietly = TRUE)) { # Added for default palette if needed
  install.packages("RColorBrewer")
}


library(quantmod)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate) # For 'years' function
library(stringr)   # For str_to_title
library(RColorBrewer) # For default palette if needed


# --- Define Your Custom Color Palette ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318" # Your orange color
COLOR_AXIS_FONT <- "#333f48"       # Your dark grey font color
# Your custom palette for the boxplot fills (used as a default or passed directly)
PALETTE_BOXPLOT_CUSTOM <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318")


#' Generate Side-by-Side Boxplots of Stock Price Performance
#'
#' This function downloads stock data, calculates returns for specified holding periods,
#' and generates side-by-side boxplots of these returns. It now incorporates a custom
#' color palette for plot elements.
#'
#' @param ticker_symbol Character string, the stock ticker symbol (e.g., "AAPL").
#' @param start_date Character string, the start date for analysis in "YYYY-MM-DD" format.
#' @param holding_periods Numeric vector, a vector of holding periods in days (e.g., c(1, 3, 5, 10)).
#'          The function is designed for up to four periods for clear side-by-side plotting.
#' @param box_colors Character vector, colors for the boxplots. Length should match `holding_periods`.
#'          Defaults to a predefined custom palette.
#' @param use_overlapping_data Logical, if TRUE, calculates overlapping period returns.
#'          If FALSE, calculates non-overlapping period returns.
#' @param return_type Character string, "discrete" for simple returns or "continuous" for log returns.
#'
#' @return A ggplot object representing the side-by-side boxplots.
#' @examples
#' \dontrun{
#'    # Example 1: Overlapping Discrete Returns for GOOG with custom palette
#'    plot1 <- generate_stock_boxplots(
#'      ticker_symbol = "GOOG",
#'      start_date = "2020-01-01",
#'      holding_periods = c(1, 5, 10, 20),
#'      box_colors = PALETTE_BOXPLOT_CUSTOM, # Using the custom palette
#'      use_overlapping_data = TRUE,
#'      return_type = "discrete"
#'    )
#'    print(plot1)
#'
#'    # Example 2: Non-Overlapping Continuous Returns for MSFT with a subset of the custom palette
#'    plot2 <- generate_stock_boxplots(
#'      ticker_symbol = "MSFT",
#'      start_date = "2019-01-01",
#'      holding_periods = c(3, 7, 15, 30),
#'      box_colors = PALETTE_BOXPLOT_CUSTOM[c(1,3,5,6)], # Using specific colors from the custom palette
#'      use_overlapping_data = FALSE,
#'      return_type = "continuous"
#'    )
#'    print(plot2)
#'
#'   # Example 3: Fewer than 4 holding periods, defaults to custom palette subset
#'    plot3 <- generate_stock_boxplots(
#'      ticker_symbol = "GOOG",
#'      start_date = "2021-01-01",
#'      holding_periods = c(1, 5),
#'      # box_colors argument is omitted, so PALETTE_BOXPLOT_CUSTOM will be used
#'      use_overlapping_data = TRUE,
#'      return_type = "continuous"
#'    )
#'    print(plot3)
#' }
generate_stock_boxplots <- function(ticker_symbol = "XOM",
                                    start_date = "2015-05-06",
                                    holding_periods = c(1, 3, 5, 10),
                                    # Default to your custom palette here
                                    box_colors = PALETTE_BOXPLOT_CUSTOM,
                                    use_overlapping_data = TRUE,
                                    return_type = "discrete") {
  
  # --- Input Validation ---
  if (length(holding_periods) > length(PALETTE_BOXPLOT_CUSTOM)) { # Changed check against actual palette size
    warning(paste("More holding periods (", length(holding_periods), ") than custom box_colors (", length(PALETTE_BOXPLOT_CUSTOM), ") provided. The plot might use default colors for extra periods or become cluttered. Using up to", length(PALETTE_BOXPLOT_CUSTOM), "periods."))
    holding_periods <- holding_periods[1:length(PALETTE_BOXPLOT_CUSTOM)]
  }
  if (length(box_colors) < length(holding_periods)) {
    # If explicitly passed box_colors are too short, use the custom palette as a fallback
    warning("Number of `box_colors` provided is less than the number of `holding_periods`. Using `PALETTE_BOXPLOT_CUSTOM`.")
    box_colors <- PALETTE_BOXPLOT_CUSTOM
  }
  if (!return_type %in% c("discrete", "continuous")) {
    stop("return_type must be either 'discrete' or 'continuous'.")
  }
  if (length(holding_periods) == 0) {
    stop("holding_periods cannot be empty.")
  }
  
  # --- 1. Download Stock Data ---
  cat("Fetching data for:", ticker_symbol, "from", start_date, "\n")
  tryCatch(
    {
      stock_data <- getSymbols(ticker_symbol,
                               src = "yahoo",
                               from = start_date,
                               auto.assign = FALSE)
    },
    error = function(e) {
      stop(paste("Failed to download data for", ticker_symbol, ":", e$message))
    }
  )
  
  # --- 2. Extract Adjusted Close Prices ---
  adj_prices <- Ad(stock_data)
  if (nrow(adj_prices) < max(holding_periods) + 1 && !use_overlapping_data) {
    stop(paste("Not enough data for the longest holding period and non-overlapping returns. Downloaded rows:", nrow(adj_prices)))
  }
  if (nrow(adj_prices) < 2 && any(holding_periods == 1)) {
    stop(paste("Not enough data for 1-day returns. Downloaded rows:", nrow(adj_prices)))
  }
  
  
  # --- 3. Calculate Returns for Each Holding Period ---
  all_returns_list <- list()
  roc_return_type <- ifelse(return_type == "continuous", "continuous", "discrete")
  
  for (i in seq_along(holding_periods)) {
    period <- holding_periods[i]
    cat("Calculating", period, "-day", return_type, "returns (Overlapping:", use_overlapping_data, ")...\n")
    
    if (period == 0) {
      warning(paste("Holding period of 0 found, skipping."))
      next
    }
    if (nrow(adj_prices) < period + 1) {
      warning(paste("Not enough data for", period, "-day holding period (", nrow(adj_prices), "rows available). Skipping this period."))
      all_returns_list[[as.character(period)]] <- NULL # Use NULL to remove it completely from list
      next
    }
    
    
    # Calculate overlapping returns using ROC (Rate of Change)
    period_returns <- ROC(adj_prices, n = period, type = roc_return_type)
    colnames(period_returns) <- paste0(period, "_Day_Return")
    
    if (!use_overlapping_data) {
      valid_returns <- na.omit(period_returns)
      if(nrow(valid_returns) > 0){
        indices <- seq(from = 1, to = nrow(valid_returns), by = period)
        period_returns <- valid_returns[indices, , drop = FALSE]
      } else {
        warning(paste("Not enough data to calculate non-overlapping returns for", period, "-day period after NA removal. Skipping."))
        period_returns <- xts::xts(matrix(numeric(0), ncol=1), order.by = Sys.Date()[0]) # empty xts
        colnames(period_returns) <- paste0(period, "_Day_Return")
      }
    }
    
    all_returns_list[[as.character(period)]] <- period_returns
  }
  
  # --- 4. Combine Data for Plotting with ggplot2 ---
  plot_data_list <- lapply(names(all_returns_list), function(period_name) {
    returns_xts <- all_returns_list[[period_name]]
    if (length(returns_xts) > 0 && !all(is.na(returns_xts))) { # Check if there's actual data
      df <- data.frame(Date = index(returns_xts), Return = coredata(returns_xts))
      colnames(df) <- c("Date", "Return") # Ensure consistent column name
      df$Holding_Period <- paste0(period_name, "-Day")
      return(df)
    }
    return(NULL) # Return NULL for empty/NA periods
  })
  
  # Remove NULL elements from the list (periods that were skipped)
  plot_data_list <- plot_data_list[!sapply(plot_data_list, is.null)]
  
  if (length(plot_data_list) == 0) {
    stop("No return data available to plot after processing all holding periods.")
  }
  
  combined_plot_data <- do.call(rbind, plot_data_list)
  
  # Ensure Holding_Period is a factor and ordered correctly for the plot
  processed_period_names <- unique(combined_plot_data$Holding_Period)
  numeric_periods_order <- sort(as.numeric(gsub("-Day", "", processed_period_names)))
  ordered_levels <- paste0(numeric_periods_order, "-Day")
  
  combined_plot_data$Holding_Period <- factor(combined_plot_data$Holding_Period, levels = ordered_levels)
  
  # --- Map box_colors to the actual holding periods being plotted ---
  # This makes sure the colors provided match the correct boxes, even if periods are skipped
  final_box_colors <- rep(NA, length(ordered_levels))
  names(final_box_colors) <- ordered_levels
  
  for(i in seq_along(holding_periods)) {
    period_label <- paste0(holding_periods[i], "-Day")
    if (period_label %in% ordered_levels && i <= length(box_colors)) {
      final_box_colors[period_label] <- box_colors[i]
    }
  }
  # Remove NAs from final_box_colors (for periods that were not plotted)
  final_box_colors <- na.omit(final_box_colors)
  # Ensure final_box_colors are in the correct order for the plot
  final_box_colors <- final_box_colors[ordered_levels]
  
  # --- 5. Generate Side-by-Side Boxplots using ggplot2 ---
  plot_title <- paste("Stock Performance for", ticker_symbol, " (Since", start_date, ")")
  plot_subtitle <- paste(stringr::str_to_title(return_type), "returns,", ifelse(use_overlapping_data, "overlapping data", "non-overlapping data"))
  y_axis_label <- paste(stringr::str_to_title(return_type), "Return")
  
  gg <- ggplot(combined_plot_data, aes(x = Holding_Period, y = Return, fill = Holding_Period)) +
    geom_boxplot(
      na.rm = TRUE,
      outlier.shape = 21,    # Matches your previous outlier style
      outlier.size = 1.5,    # Matches your previous outlier style
      outlier.alpha = 0.5,   # Matches your previous outlier style
      color = COLOR_AXIS_LINES_TITLE # Sets the border color of the boxes
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
    scale_fill_manual(values = final_box_colors) + # Use the filtered and ordered colors
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Holding Period",
      y = y_axis_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      # Plot background
      plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      
      # Titles
      plot.title = element_text(hjust = 0.5, face = "bold", color = COLOR_AXIS_LINES_TITLE),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = COLOR_AXIS_FONT), # Increased subtitle size slightly
      
      # Axis text and titles
      axis.text.x = element_text(angle = 45, hjust = 1, color = COLOR_AXIS_FONT),
      axis.text.y = element_text(color = COLOR_AXIS_FONT),
      axis.title.x = element_text(color = COLOR_AXIS_FONT),
      axis.title.y = element_text(color = COLOR_AXIS_FONT),
      
      # Axis lines (tick marks and line along axis)
      axis.line = element_line(color = COLOR_AXIS_LINES_TITLE), # Colors the actual axis line
      axis.ticks = element_line(color = COLOR_AXIS_LINES_TITLE), # Colors the tick marks
      
      # Panel grid lines
      panel.grid.major = element_line(color = "#e0e0e0", linetype = "dotted"), # Light grey for subtle grid
      panel.grid.minor = element_blank(), # No minor grid lines
      
      # Legend (not needed as fill is by x-axis)
      legend.position = "none",
      
      # Plot margins (optional adjustment)
      plot.margin = unit(c(1, 1, 1, 1), "lines") # Top, Right, Bottom, Left
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) # Format y-axis as percentage
  
  return(gg)
}

# --- How to Use with your Custom Colors ---
# Define your custom palette explicitly for calling the function if you wish
# to override the default in the function definition or for clarity.
PALETTE_BOXPLOT_FOR_CALL <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318")

# Example 1: Overlapping Discrete Returns for GOOG with custom palette
try({
  plot_goog_discrete_overlap <- generate_stock_boxplots(
    ticker_symbol = "AAPL",
    start_date = "2014-03-01",
    holding_periods = c(1, 5, 10, 20),
    box_colors = PALETTE_BOXPLOT_FOR_CALL, # Pass your custom palette
    use_overlapping_data = TRUE,
    return_type = "discrete"
  )
  print(plot_goog_discrete_overlap)
})

# Example 2: Non-Overlapping Continuous Returns for MSFT with a subset of the custom palette
try({
  plot_msft_continuous_nonoverlap <- generate_stock_boxplots(
    ticker_symbol = "MSFT",
    start_date = "2019-01-01",
    holding_periods = c(3, 7, 15, 30),
    box_colors = PALETTE_BOXPLOT_FOR_CALL[c(1,3,5,6)], # Use specific colors from your custom palette
    use_overlapping_data = FALSE,
    return_type = "continuous"
  )
  print(plot_msft_continuous_nonoverlap)
})

# Example 3: Fewer than 4 holding periods, defaults to custom palette subset (by omitting box_colors)
try({
  plot_goog_2periods <- generate_stock_boxplots(
    ticker_symbol = "GOOG",
    start_date = "2021-01-01",
    holding_periods = c(5, 20),
    # box_colors argument is omitted, so PALETTE_BOXPLOT_CUSTOM defined in the function will be used
    use_overlapping_data = TRUE,
    return_type = "continuous"
  )
  print(plot_goog_2periods)
})


# Example 4: Case with insufficient data for some periods (colors will adapt)
try({
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  plot_short_data <- generate_stock_boxplots(
    ticker_symbol = "NVDA",
    start_date = paste0(current_year - 1 , "-10-01"), # Approx 6-7 months of data from today
    holding_periods = c(1, 10, 30, 60), # 60-day non-overlapping might be an issue
    box_colors = PALETTE_BOXPLOT_FOR_CALL, # Provide your palette, function handles subsetting
    use_overlapping_data = FALSE,
    return_type = "continuous"
  )
  print(plot_short_data)
})

# Example 5: Very short data period overall (colors will adapt)
try({
  plot_very_short_data <- generate_stock_boxplots(
    ticker_symbol = "YUM",
    start_date = format(Sys.Date() - 15, "%Y-%m-%d"), # Only last 15 days
    holding_periods = c(1, 3, 5),
    box_colors = PALETTE_BOXPLOT_FOR_CALL, # Provide your palette, function handles subsetting
    use_overlapping_data = TRUE,
    return_type = "discrete"
  )
  print(plot_very_short_data)
})