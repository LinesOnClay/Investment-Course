# --- Install and Load Necessary Packages ---
# These lines check if packages are installed and install them if they're not.
# They will install automatically if needed, then load the libraries.
if (!requireNamespace("yfR", quietly = TRUE)) { # Package for fetching Yahoo Finance data (for stocks)
  install.packages("yfR")
}
if (!requireNamespace("quantmod", quietly = TRUE)) { # Package for fetching FRED data (no API key needed for basic FRED)
  install.packages("quantmod")
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
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("RcppRoll", quietly = TRUE)) { # Package for efficient rolling calculations (for stocks)
  install.packages("RcppRoll")
}
if (!requireNamespace("scales", quietly = TRUE)) { # Package for axis formatting (percentages)
  install.packages("scales")
}


# Load the packages required for the script
library(yfR)       # For fetching stock/ETF data from Yahoo Finance
library(quantmod)  # For fetching economic data from FRED
library(ggplot2)   # For creating professional-looking plots
library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping data
library(lubridate) # For robust date handling
library(RcppRoll)  # For fast rolling products (used with stock returns)
library(scales)    # For formatting axis labels (e.g., as percentages)

# --- Define Global Color Palette ---
# These are the custom colors specified for the plot's aesthetics.
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
COLOR_KERNEL_DENSITY <- "#e8c547" # Not currently used, but kept in palette.

# Colors for the histogram fills (customized for better contrast)
COLOR_PRICE_ONLY_HIST <- "#2a6ca3"   # Distinct blue for Price-Only stock returns
COLOR_TOTAL_RETURN_HIST <- "#4CAF50" # Distinct green for Total stock returns
COLOR_FRED_YIELD_HIST <- "#800080"   # New color for FRED yield histogram (Purple)

# --- USER INPUTS: EDIT THESE VALUES DIRECTLY ---
# Choose your data source: "stock" or "fred"
data_source <- "stock" # Set to "stock" for stocks/ETFs, or "fred" for FRED data

# If data_source is "stock": asset_symbol should be a stock/ETF ticker (e.g., "SPY", "AAPL")
# If data_source is "fred": asset_symbol should be a FRED series ID (e.g., "DGS10", "DGS3MO")
asset_symbol <- "SPY"

# Start and End dates for fetching data
# For "stock", start_date_input needs to be ~1 year before your desired rolling return start.
# For "fred", start_date_input is the actual start of the yield data you want to analyze.
start_date_input <- "2010-01-01"
end_date_input <- as.character(Sys.Date()) # Defaults to today's date

# --- Function to Get and Process Data ---
# This function fetches data based on the chosen source and processes it.
get_and_process_returns <- function(symbol, start_date_str_raw, end_date_str_raw, source_type) {
  # Trim any leading/trailing whitespace from the input date strings
  start_date_clean <- trimws(start_date_str_raw)
  end_date_clean <- trimws(end_date_str_raw)
  
  # Debugging messages to show what dates are being processed (visible in console)
  cat(paste0("DEBUG: Cleaned Start Date String: '", start_date_clean, "'\n"))
  cat(paste0("DEBUG: Cleaned End Date String:   '", end_date_clean, "'\n"))
  
  # Explicitly convert the cleaned strings to Date objects using the specified format.
  start_date_obj <- as.Date(start_date_clean, format = "%Y-%m-%d")
  end_date_obj <- as.Date(end_date_clean, format = "%Y-%m-%d")
  
  # Validate the converted Date objects
  if (is.na(start_date_obj)) {
    stop(paste0("Invalid start date format: '", start_date_str_raw, "'. Please use Букмекерлар-MM-DD."))
  }
  if (is.na(end_date_obj)) {
    stop(paste0("Invalid end date format: '", end_date_str_raw, "'. Please use Букмекерлар-MM-DD."))
  }
  if (start_date_obj > end_date_obj) {
    stop(paste0("Start date (", start_date_obj, ") cannot be after end date (", end_date_obj, "). Please adjust your dates."))
  }
  
  cat(paste0("Fetching data for ", symbol, " from ", start_date_obj, " to ", end_date_obj, " using ", source_type, " source...\n"))
  
  # Define the rolling window size for stocks (approx. trading days in a year)
  ROLLING_WINDOW_DAYS <- 252
  
  tryCatch({
    if (source_type == "stock") {
      # --- STOCK/ETF DATA FETCHING AND PROCESSING (yfR) ---
      data_yfr <- yf_get(
        tickers = symbol,
        first_date = start_date_obj,
        last_date = end_date_obj,
        freq_data = "daily"
      )
      
      cat(paste0("DEBUG: [1] Rows in data_yfr immediately after yf_get: ", NROW(data_yfr), "\n"))
      
      # Filter out rows with NA for price_adjusted and price_close.
      data_yfr <- data_yfr %>%
        filter(!is.na(price_adjusted) & !is.na(price_close))
      
      cat(paste0("DEBUG: [2] Rows in data_yfr after NA price filters: ", NROW(data_yfr), "\n"))
      
      if (NROW(data_yfr) < ROLLING_WINDOW_DAYS) {
        stop(paste0("Not enough valid data points (at least ", ROLLING_WINDOW_DAYS, " needed) to calculate rolling annual returns for the given period. Please choose a longer period or different dates for stock data."))
      }
      
      # Calculate Daily Returns (Total and Price-Only)
      daily_returns_calc <- data_yfr %>%
        arrange(ref_date) %>%
        mutate(
          prev_adjusted = lag(price_adjusted, n = 1),
          prev_close = lag(price_close, n = 1),
          
          TotalReturn_daily = (price_adjusted / prev_adjusted) - 1, # Correct: Total uses price_adjusted
          PriceReturn_daily = (price_close / prev_close) - 1        # Correct: Price-Only uses price_close
        ) %>%
        filter(!is.na(TotalReturn_daily) & !is.na(PriceReturn_daily)) # Filter out first day's NA returns
      
      cat(paste0("DEBUG: [3] Rows in daily_returns_calc after daily returns: ", NROW(daily_returns_calc), "\n"))
      
      # Calculate Rolling Annual Returns
      rolling_annual_returns <- daily_returns_calc %>%
        mutate(
          TotalAnnualReturn = RcppRoll::roll_prod(1 + TotalReturn_daily, n = ROLLING_WINDOW_DAYS, align = "right", fill = NA) - 1,
          PriceAnnualReturn = RcppRoll::roll_prod(1 + PriceReturn_daily, n = ROLLING_WINDOW_DAYS, align = "right", fill = NA) - 1
        ) %>%
        filter(!is.na(TotalAnnualReturn) & !is.na(PriceAnnualReturn))
      
      cat(paste0("DEBUG: [4] Rows in rolling_annual_returns after annual calcs: ", NROW(rolling_annual_returns), "\n"))
      
      if (NROW(rolling_annual_returns) == 0) {
        stop("No data left after calculating rolling annual returns. Ensure your stock date range is long enough.")
      }
      
      # Reshape for plotting (Total vs Price-Only for stocks)
      df_long <- rolling_annual_returns %>%
        select(Date = ref_date, TotalAnnualReturn, PriceAnnualReturn) %>%
        pivot_longer(
          cols = c(TotalAnnualReturn, PriceAnnualReturn),
          names_to = "ReturnComponent",
          values_to = "Return"
        ) %>%
        mutate(ReturnComponent = factor(ReturnComponent, levels = c("TotalAnnualReturn", "PriceAnnualReturn"))) %>%
        filter(!is.na(Return))
      
      cat(paste0("DEBUG: [5] Rows in df_long for stock data after final processing: ", NROW(df_long), "\n"))
      
      # Attach source type attribute for plotting function
      attr(df_long, "source_type") <- "stock"
      return(df_long)
      
    } else if (source_type == "fred") {
      # --- FRED DATA FETCHING AND PROCESSING (quantmod) ---
      # getSymbols from quantmod handles FRED data directly.
      fred_data_xts <- getSymbols(symbol, src = "FRED", from = start_date_obj, to = end_date_obj, auto.assign = FALSE)
      
      cat(paste0("DEBUG: [1F] Rows in fred_data_xts immediately after getSymbols: ", NROW(fred_data_xts), "\n"))
      
      if (NROW(fred_data_xts) == 0) {
        stop(paste0("No data found for FRED series '", symbol, "'. Please check the FRED series ID or date range."))
      }
      
      # FRED data often has NA values for weekends/holidays or missing observations.
      fred_data_df <- data.frame(
        Date = index(fred_data_xts),
        Value = as.numeric(fred_data_xts[,1]) # Take the first column of the FRED series
      ) %>%
        filter(!is.na(Value)) # Remove NA values
      
      cat(paste0("DEBUG: [2F] Rows in fred_data_df after NA filter: ", NROW(fred_data_df), "\n"))
      
      if (NROW(fred_data_df) < 2) {
        stop(paste0("Not enough valid data points for FRED series '", symbol, "' after filtering. Please choose a longer period."))
      }
      
      # For FRED data, we plot the distribution of the values themselves.
      df_long <- fred_data_df %>%
        mutate(ReturnComponent = "Yield") %>% # Use "Yield" as the component name for FRED data
        rename(Return = Value) %>%            # Rename 'Value' to 'Return' for consistency with plotting function
        select(Date, ReturnComponent, Return)
      
      cat(paste0("DEBUG: [3F] Rows in df_long for FRED data after final processing: ", NROW(df_long), "\n"))
      
      # Attach source type attribute for plotting function
      attr(df_long, "source_type") <- "fred"
      return(df_long)
      
    } else {
      stop("Invalid data_source specified. Please use 'stock' or 'fred'.")
    }
    
  }, error = function(e) {
    message(paste("Error fetching or processing data for", symbol, " (Source: ", source_type, "):", e$message))
    message("Please check the symbol/series ID, date range, or internet connection.")
    return(NULL)
  })
}

# --- Function to Plot Distribution (UPDATED for Stock/FRED Differentiation and On-Plot Stats) ---
# This function generates the ggplot2 visualization of return/yield distributions.
plot_return_distribution <- function(data_df, symbol, start_date_str, end_date_str,
                                     # Stock stats
                                     mean_price = NULL, sd_price = NULL, q1_price = NULL, median_price = NULL, q3_price = NULL, iqr_price = NULL,
                                     mean_total = NULL, sd_total = NULL, q1_total = NULL, median_total = NULL, q3_total = NULL, iqr_total = NULL,
                                     # FRED stats
                                     mean_yield = NULL, sd_yield = NULL, q1_yield = NULL, median_yield = NULL, q3_yield = NULL, iqr_yield = NULL) {
  
  # Check if data is valid for plotting
  if (is.null(data_df) || NROW(data_df) == 0) {
    message("No valid data available to plot the distribution.")
    return(NULL)
  }
  
  source_type <- attr(data_df, "source_type") # Get the source type from data attribute
  
  # Base plot setup (common to both)
  p <- ggplot(data_df, aes(x = Return, fill = ReturnComponent)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 10, hjust = 0.5),
      axis.line = element_line(color = COLOR_AXIS_LINES_TITLE),
      axis.ticks = element_line(color = COLOR_AXIS_LINES_TITLE),
      axis.text = element_text(color = COLOR_AXIS_FONT),
      axis.title = element_text(color = COLOR_AXIS_FONT, face = "bold"),
      legend.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      legend.text = element_text(color = COLOR_AXIS_FONT),
      legend.title = element_text(color = COLOR_AXIS_FONT, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  if (source_type == "stock") {
    # --- Plotting for Stock/ETF Returns ---
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = 0.0075, # Adjusted binwidth for annual returns (0.75%)
                     alpha = 0.6,      # Translucency for overlap
                     position = "identity", # Allow bars to overlap
                     color = "white") + # White borders
      scale_fill_manual(values = c("TotalAnnualReturn" = COLOR_TOTAL_RETURN_HIST, "PriceAnnualReturn" = COLOR_PRICE_ONLY_HIST),
                        labels = c("Total Annual Return", "Price-Only Annual Return")) +
      scale_x_continuous(labels = scales::percent) + # Format x-axis as percentages
      labs(
        title = paste("Distribution of Rolling Annual Returns for", toupper(symbol)),
        subtitle = paste("Data fetched from:", start_date_str, "to", end_date_str, "(Rolling Window: ~1 Year)"),
        x = "Rolling Annual Return",
        y = "Density",
        fill = "Return Type"
      )
    
    # Add statistics annotations for Stock data
    if (!is.null(mean_total) && !is.null(sd_total)) {
      stats_text_total <- paste0(
        "Total Return:\n",
        "  Mean: ", scales::percent(mean_total, accuracy = 0.01), "\n",
        "  SD:   ", scales::percent(sd_total, accuracy = 0.01), "\n",
        "  Q1:   ", scales::percent(q1_total, accuracy = 0.01), "\n",
        "  Med:  ", scales::percent(median_total, accuracy = 0.01), "\n",
        "  Q3:   ", scales::percent(q3_total, accuracy = 0.01), "\n",
        "  IQR:  ", scales::percent(iqr_total, accuracy = 0.01)
      )
      
      stats_text_price <- paste0(
        "Price-Only Return:\n",
        "  Mean: ", scales::percent(mean_price, accuracy = 0.01), "\n",
        "  SD:   ", scales::percent(sd_price, accuracy = 0.01), "\n",
        "  Q1:   ", scales::percent(q1_price, accuracy = 0.01), "\n",
        "  Med:  ", scales::percent(median_price, accuracy = 0.01), "\n",
        "  Q3:   ", scales::percent(q3_price, accuracy = 0.01), "\n",
        "  IQR:  ", scales::percent(iqr_price, accuracy = 0.01)
      )
      
      # Determine x-axis limits for consistent annotation placement
      x_min <- min(data_df$Return, na.rm = TRUE)
      x_max <- max(data_df$Return, na.rm = TRUE)
      
      # Use ggplot_build to get current plot limits for annotation positioning
      # This ensures stats are visible even if data range is very narrow or wide
      y_range_max <- max(ggplot_build(ggplot(data_df, aes(x = Return, fill = ReturnComponent)) +
                                        geom_histogram(aes(y = after_stat(density)), binwidth = 0.0075, position = "identity"))$data[[1]]$density, na.rm = TRUE)
      
      # Positioning for annotations (top-left) - adjust multiplier as needed
      x_pos_stats <- x_min + (x_max - x_min) * 0.02
      y_pos_total_stats <- y_range_max * 0.98 # Top-most position
      y_pos_price_stats <- y_range_max * 0.60 # Lower position for price stats to avoid overlap
      
      p <- p +
        annotate("text", x = x_pos_stats, y = y_pos_total_stats,
                 label = stats_text_total, hjust = 0, vjust = 1, lineheight = 0.85, # lineheight for tighter spacing
                 color = COLOR_TOTAL_RETURN_HIST, fontface = "bold", size = 3.5) + # size adjusted for more lines
        annotate("text", x = x_pos_stats, y = y_pos_price_stats,
                 label = stats_text_price, hjust = 0, vjust = 1, lineheight = 0.85,
                 color = COLOR_PRICE_ONLY_HIST, fontface = "bold", size = 3.5)
    }
    
  } else if (source_type == "fred") {
    # --- Plotting for FRED Yields ---
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = 0.001, # Adjusted binwidth for yield percentages (0.1%) - yields are usually smaller values
                     alpha = 0.8,
                     color = "white",
                     fill = COLOR_FRED_YIELD_HIST) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) + # Format x-axis as percentages
      labs(
        title = paste("Distribution of US Treasury Yields for", symbol),
        subtitle = paste("Data fetched from FRED:", start_date_str, "to", end_date_str),
        x = "Yield (%)",
        y = "Density",
        fill = "Yield Type"
      )
    
    # Add statistics annotations for FRED data
    if (!is.null(mean_yield) && !is.null(sd_yield)) {
      stats_text_yield <- paste0(
        "Yield Distribution:\n",
        "  Mean: ", scales::percent(mean_yield, accuracy = 0.01), "\n", # Format as percentage
        "  SD:   ", scales::percent(sd_yield, accuracy = 0.01), "\n", # Format as percentage
        "  Q1:   ", scales::percent(q1_yield, accuracy = 0.01), "\n",
        "  Med:  ", scales::percent(median_yield, accuracy = 0.01), "\n",
        "  Q3:   ", scales::percent(q3_yield, accuracy = 0.01), "\n",
        "  IQR:  ", scales::percent(iqr_yield, accuracy = 0.01)
      )
      
      # Determine x-axis limits for consistent annotation placement
      x_min <- min(data_df$Return, na.rm = TRUE)
      x_max <- max(data_df$Return, na.rm = TRUE)
      
      y_range_max <- max(ggplot_build(ggplot(data_df, aes(x = Return)) +
                                        geom_histogram(aes(y = after_stat(density)), binwidth = 0.001))$data[[1]]$density, na.rm = TRUE)
      
      x_pos_stats <- x_min + (x_max - x_min) * 0.02
      y_pos_yield_stats <- y_range_max * 0.98 # Top-most position
      
      p <- p +
        annotate("text", x = x_pos_stats, y = y_pos_yield_stats,
                 label = stats_text_yield, hjust = 0, vjust = 1, lineheight = 0.85,
                 color = COLOR_FRED_YIELD_HIST, fontface = "bold", size = 3.5)
    }
    
  } else {
    message("Unknown data source type for plotting.")
    return(NULL)
  }
  
  print(p) # Explicitly print the plot object
}

# --- Main Execution Block ---
# This is the starting point of the script's execution.
# It uses the directly specified inputs to fetch data and generate the plot.

# Call the 'get_and_process_returns' function to fetch and prepare the data.
returns_data <- get_and_process_returns(
  symbol = toupper(asset_symbol),
  start_date_str_raw = start_date_input,
  end_date_str_raw = end_date_input,
  source_type = data_source
)

# After data processing, check if 'returns_data' is not NULL (meaning no errors occurred).
# If data is valid, proceed to plot and calculate statistics.
if (!is.null(returns_data)) {
  # Get the source type from the returned data for conditional statistics calculation
  source_type_actual <- attr(returns_data, "source_type")
  
  if (source_type_actual == "stock") {
    # --- Calculate Statistics for Stock/ETF Data ---
    cat("\n--- Stock/ETF Return Statistics (Rolling Annual) ---\n")
    
    price_returns <- returns_data %>%
      filter(ReturnComponent == "PriceAnnualReturn") %>%
      pull(Return)
    
    total_returns <- returns_data %>%
      filter(ReturnComponent == "TotalAnnualReturn") %>%
      pull(Return)
    
    # Calculate statistics for Price-Only
    mean_price <- mean(price_returns, na.rm = TRUE)
    sd_price <- sd(price_returns, na.rm = TRUE)
    q_price <- quantile(price_returns, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    q1_price <- q_price[1]
    median_price <- q_price[2]
    q3_price <- q_price[3]
    iqr_price <- IQR(price_returns, na.rm = TRUE)
    
    # Calculate statistics for Total Return
    mean_total <- mean(total_returns, na.rm = TRUE)
    sd_total <- sd(total_returns, na.rm = TRUE)
    q_total <- quantile(total_returns, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    q1_total <- q_total[1]
    median_total <- q_total[2]
    q3_total <- q_total[3]
    iqr_total <- IQR(total_returns, na.rm = TRUE)
    
    # Generate the plot, passing all stock statistics
    plot_return_distribution(
      data_df = returns_data,
      symbol = toupper(asset_symbol),
      start_date_str = start_date_input,
      end_date_str = end_date_input,
      mean_price = mean_price,
      sd_price = sd_price,
      q1_price = q1_price,
      median_price = median_price,
      q3_price = q3_price,
      iqr_price = iqr_price,
      mean_total = mean_total,
      sd_total = sd_total,
      q1_total = q1_total,
      median_total = median_total,
      q3_total = q3_total,
      iqr_total = iqr_total
    )
    
    # Print to console as well for detailed view
    cat(paste0("Asset: ", toupper(asset_symbol), "\n"))
    cat(paste0("Period: ", start_date_input, " to ", end_date_input, " (Rolling Window: ~1 Year)\n"))
    cat("------------------------------------------\n")
    cat(paste0("Price-Only Annual Return:\n"))
    cat(paste0("  Mean: ", sprintf("%.4f", mean_price), " (", sprintf("%.2f%%", mean_price * 100), ")\n"))
    cat(paste0("  Std Dev: ", sprintf("%.4f", sd_price), " (", sprintf("%.2f%%", sd_price * 100), ")\n"))
    cat(paste0("  Q1: ", sprintf("%.4f", q1_price), " (", sprintf("%.2f%%", q1_price * 100), ")\n"))
    cat(paste0("  Median: ", sprintf("%.4f", median_price), " (", sprintf("%.2f%%", median_price * 100), ")\n"))
    cat(paste0("  Q3: ", sprintf("%.4f", q3_price), " (", sprintf("%.2f%%", q3_price * 100), ")\n"))
    cat(paste0("  IQR: ", sprintf("%.4f", iqr_price), " (", sprintf("%.2f%%", iqr_price * 100), ")\n"))
    cat("------------------------------------------\n")
    cat(paste0("Total Annual Return (with Dividends):\n"))
    cat(paste0("  Mean: ", sprintf("%.4f", mean_total), " (", sprintf("%.2f%%", mean_total * 100), ")\n"))
    cat(paste0("  Std Dev: ", sprintf("%.4f", sd_total), " (", sprintf("%.2f%%", sd_total * 100), ")\n"))
    cat(paste0("  Q1: ", sprintf("%.4f", q1_total), " (", sprintf("%.2f%%", q1_total * 100), ")\n"))
    cat(paste0("  Median: ", sprintf("%.4f", median_total), " (", sprintf("%.2f%%", median_total * 100), ")\n"))
    cat(paste0("  Q3: ", sprintf("%.4f", q3_total), " (", sprintf("%.2f%%", q3_total * 100), ")\n"))
    cat(paste0("  IQR: ", sprintf("%.4f", iqr_total), " (", sprintf("%.2f%%", iqr_total * 100), ")\n"))
    cat("------------------------------------------\n")
    
  } else if (source_type_actual == "fred") {
    # --- Calculate Statistics for FRED Yield Data ---
    cat("\n--- FRED Yield Statistics ---\n")
    
    yield_values <- returns_data %>%
      filter(ReturnComponent == "Yield") %>%
      pull(Return)
    
    mean_yield <- mean(yield_values, na.rm = TRUE)
    sd_yield <- sd(yield_values, na.rm = TRUE)
    q_yield <- quantile(yield_values, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    q1_yield <- q_yield[1]
    median_yield <- q_yield[2]
    q3_yield <- q_yield[3]
    iqr_yield <- IQR(yield_values, na.rm = TRUE)
    
    
    # Generate the plot, passing FRED statistics
    plot_return_distribution(
      data_df = returns_data,
      symbol = toupper(asset_symbol),
      start_date_str = start_date_input,
      end_date_str = end_date_input,
      mean_yield = mean_yield,
      sd_yield = sd_yield,
      q1_yield = q1_yield,
      median_yield = median_yield,
      q3_yield = q3_yield,
      iqr_yield = iqr_yield
    )
    
    # Print to console as well for detailed view
    cat(paste0("FRED Series: ", toupper(asset_symbol), "\n"))
    cat(paste0("Period: ", start_date_input, " to ", end_date_input, "\n"))
    cat("------------------------------------------\n")
    cat(paste0("Yield Distribution:\n"))
    cat(paste0("  Mean: ", sprintf("%.4f", mean_yield), " (", sprintf("%.2f%%", mean_yield * 100), ")\n"))
    cat(paste0("  Std Dev: ", sprintf("%.4f", sd_yield), " (", sprintf("%.2f%%", sd_yield * 100), ")\n"))
    cat(paste0("  Q1: ", sprintf("%.4f", q1_yield), " (", sprintf("%.2f%%", q1_yield * 100), ")\n"))
    cat(paste0("  Median: ", sprintf("%.4f", median_yield), " (", sprintf("%.2f%%", median_yield * 100), ")\n"))
    cat(paste0("  Q3: ", sprintf("%.4f", q3_yield), " (", sprintf("%.2f%%", q3_yield * 100), ")\n"))
    cat(paste0("  IQR: ", sprintf("%.4f", iqr_yield), " (", sprintf("%.2f%%", iqr_yield * 100), ")\n"))
    cat("------------------------------------------\n")
    
  } else {
    message("Plot cannot be generated: Unknown source type for statistics and plotting.")
  }
  
} else {
  # If data processing failed, inform the user that the plot cannot be generated.
  message("Plot cannot be generated due to data retrieval or processing issues.")
  message("Please review the messages above for specific errors (e.g., symbol validity, date range).")
}
