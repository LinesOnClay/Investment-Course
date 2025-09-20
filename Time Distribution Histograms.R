
# --- USER INPUTS: EDIT THESE VALUES DIRECTLY ---

# If data_source is "stock": asset_symbol should be a stock/ETF ticker (e.g., "SPY", "AAPL")
# If data_source is "fred": (Not fully supported for growth of $100 in this version)
asset_symbol <- "VUG"

# Start and End dates for fetching data
# For longer horizons (e.g., 20 years), ensure start_date_input is far enough back.
start_date_input <- "2006-01-01" # Set to an early date for longer horizons
end_date_input <- as.character(Sys.Date()) # Defaults to today's date



# Choose your data source: "stock" or "fred"
# This script is primarily designed for "stock" growth.
data_source <- "stock"



#__________________________________________________________________________________



# --- Install and Load Necessary Packages ---
# These lines check if packages are installed and install them if they're not.
# They will install automatically if needed, then load the libraries.
if (!requireNamespace("yfR", quietly = TRUE)) { # Package for fetching Yahoo Finance data (for stocks)
  install.packages("yfR")
}
if (!requireNamespace("quantmod", quietly = TRUE)) { # Package for fetching FRED data
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
if (!requireNamespace("RcppRoll", quietly = TRUE)) { # Package for efficient rolling calculations
  install.packages("RcppRoll")
}
if (!requireNamespace("scales", quietly = TRUE)) { # Package for axis formatting (percentages)
  install.packages("scales")
}
if (!requireNamespace("readr", quietly = TRUE)) { # For robust numeric parsing
  install.packages("readr")
}

# Load the packages required for the script
library(yfR)       # For fetching stock/ETF data from Yahoo Finance
library(quantmod)  # For fetching economic data from FRED
library(ggplot2)   # For creating professional-looking plots
library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping data
library(lubridate) # For robust date handling
library(RcppRoll)  # For fast rolling products
library(scales)    # For formatting axis labels (e.g., as percentages)
library(readr)     # For robust numeric parsing




# --- Analysis Horizons (in years) ---
# Define the periods over which to calculate growth.
# This will determine the facets in the plot.
ANALYSIS_HORIZONS_NUMERIC <- c(0.5, 1, 2, 3, 5, 7, 10, 15, 20)
# Create corresponding labels for facets and tables, ensuring correct order
ANALYSIS_HORIZONS_LABELS <- c("6-Month Growth", "1-Year Growth", "2-Year Growth", "3-Year Growth",
                              "5-Year Growth", "7-Year Growth", "10-Year Growth",
                              "15-Year Growth", "20-Year Growth")


# --- Define Global Color Palette ---
# These are the custom colors specified for the plot's aesthetics.
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
COLOR_KERNEL_DENSITY <- "#e8c547" # Not currently used.

# Color for the growth of $100 histograms (Total Return only)
COLOR_TOTAL_RETURN_HIST <- "#2a6ca3" # A distinct green


# --- Function to Get and Process Growth Data ---
# This function fetches data based on the chosen source and processes it into growth of $100.
get_growth_data <- function(symbol, start_date_str_raw, end_date_str_raw, source_type, horizon_years) {
  # Trim whitespace and convert dates
  start_date_clean <- trimws(start_date_str_raw)
  end_date_clean <- trimws(end_date_str_raw)
  start_date_obj <- as.Date(start_date_clean, format = "%Y-%m-%d")
  end_date_obj <- as.Date(end_date_clean, format = "%Y-%m-%d")
  
  # Validate dates
  if (is.na(start_date_obj) || is.na(end_date_obj) || start_date_obj > end_date_obj) {
    stop(paste0("Invalid date range specified: '", start_date_str_raw, "' to '", end_date_str_raw, "'. Please use YYYY-MM-DD and ensure start date is before end date."))
  }
  
  cat(paste0("Fetching data for ", symbol, " from ", start_date_obj, " to ", end_date_obj, " for ", horizon_years, "-year growth using ", source_type, " source...\n"))
  
  # Approximate trading days in a year (for rolling window)
  TRADING_DAYS_PER_YEAR <- 252
  horizon_days <- round(horizon_years * TRADING_DAYS_PER_YEAR) # Round to nearest day
  
  tryCatch({
    if (source_type == "stock") {
      # --- STOCK/ETF DATA FETCHING AND GROWTH CALCULATION (yfR) ---
      data_yfr <- yf_get(
        tickers = symbol,
        first_date = start_date_obj,
        last_date = end_date_obj,
        freq_data = "daily" # Daily frequency is crucial for rolling calculations
      )
      
      cat(paste0("DEBUG: [yfR Get] Rows after yf_get for ", symbol, ": ", NROW(data_yfr), "\n"))
      if (NROW(data_yfr) == 0) {
        stop(paste0("yfR returned no data for '", symbol, "'. Check ticker or date range. It might also be a temporary Yahoo Finance issue."))
      }
      
      # Filter out rows with NA for adjusted close price.
      data_yfr <- data_yfr %>%
        filter(!is.na(price_adjusted))
      
      cat(paste0("DEBUG: [Price Filter] Rows after NA price_adjusted filter: ", NROW(data_yfr), "\n"))
      
      if (NROW(data_yfr) < horizon_days) {
        stop(paste0("Not enough valid data points (at least ", horizon_days, " needed for a ", horizon_years, "-year period) to calculate growth. Please choose a longer period or different dates for stock data."))
      }
      
      # Calculate Daily Total Return using adjusted prices (includes dividends)
      daily_returns_calc <- data_yfr %>%
        arrange(ref_date) %>% # Order by date for correct lag calculation
        mutate(
          prev_adjusted = lag(price_adjusted, n = 1),
          TotalReturn_daily = (price_adjusted / prev_adjusted) - 1
        ) %>%
        filter(!is.na(TotalReturn_daily)) # Remove the first row which will have NA due to lag
      
      cat(paste0("DEBUG: [Daily Returns] Rows after daily return calculation: ", NROW(daily_returns_calc), "\n"))
      
      
      # Calculate Rolling Compounded Total Return over the specified horizon
      growth_data_df <- daily_returns_calc %>%
        mutate(
          CompoundedReturn = RcppRoll::roll_prod(1 + TotalReturn_daily, n = horizon_days, align = "right", fill = NA) - 1
        ) %>%
        filter(!is.na(CompoundedReturn)) %>% # Filter out initial NA values from rolling product
        mutate(EndingValue = 100 * (1 + CompoundedReturn)) %>%
        # Use case_when to apply specific label for 6-month
        mutate(Horizon = case_when(horizon_years == 0.5 ~ "6-Month Growth",
                                   TRUE ~ paste0(horizon_years, "-Year Growth"))) %>%
        select(Date = ref_date, EndingValue, Horizon) # Select final relevant columns
      
      cat(paste0("DEBUG: [Growth Calc] Rows after rolling growth calculation: ", NROW(growth_data_df), "\n"))
      
      if (NROW(growth_data_df) == 0) {
        stop(paste0("No data left after calculating ", horizon_years, "-year rolling growth. Ensure your stock date range is long enough."))
      }
      
      # Extract actual data range used for these calculations
      actual_data_start_date <- min(data_yfr$ref_date, na.rm = TRUE)
      actual_data_end_date <- max(data_yfr$ref_date, na.rm = TRUE)
      
      # Attach actual start/end dates as attributes
      attr(growth_data_df, "actual_start_date") <- actual_data_start_date
      attr(growth_data_df, "actual_end_date") <- actual_data_end_date
      
      return(growth_data_df)
      
    } else if (source_type == "fred") {
      # --- FRED DATA (NOT CURRENTLY SUPPORTED FOR GROWTH OF $100 IN THIS SCRIPT) ---
      stop("Growth of $100 is not supported for 'fred' data_source in this script. Please set data_source to 'stock'.")
      
    } else {
      stop("Invalid data_source specified. Please use 'stock' or 'fred'.")
    }
    
  }, error = function(e) {
    message(paste0("Error fetching or processing data for ", symbol, " (Source: ", source_type, ", Horizon: ", horizon_years, " year(s)): ", e$message))
    message("Please check the symbol/series ID, date range, or internet connection.")
    return(NULL) # Return NULL to indicate failure
  })
}

# --- Function to Plot Growth Distribution ---
# This function generates a faceted ggplot2 visualization of growth distributions.
plot_growth_distribution <- function(data_df, symbol, horizons_labels, overall_start_date, overall_end_date) {
  # Check if data is valid for plotting
  if (is.null(data_df) || NROW(data_df) == 0) {
    message("No valid data available to plot the distribution.")
    return(NULL)
  }
  
  # Use the passed overall dates for subtitle
  # actual_start_date <- attr(data_df, "actual_start_date") # REMOVED
  # actual_end_date <- attr(data_df, "actual_end_date")     # REMOVED
  
  # Ensure Horizon is a factor for consistent ordering in facets
  data_df$Horizon <- factor(data_df$Horizon, levels = horizons_labels)
  
  # Create the ggplot2 plot
  p <- ggplot(data_df, aes(x = EndingValue)) +
    # Histogram of ending values
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = 5, # Binwidth for ending value ($5 bins, adjust as needed)
                   alpha = 0.8,
                   fill = COLOR_TOTAL_RETURN_HIST, # Using the Total Return color
                   color = "white") + # White borders
    
    # Facet by Horizon (e.g., 6-Month, 1-Year, etc.)
    facet_wrap(~ Horizon, scales = "free_x", ncol = 3) + # Set to 3 columns for 3x3 layout
    
    # Scale X-axis for currency/dollar values
    scale_x_continuous(labels = scales::dollar) +
    
    # Labs for titles and labels
    labs(
      title = paste("Distribution of $100 Growth for", toupper(symbol)),
      subtitle = paste("Data from:", overall_start_date, "to", overall_end_date), # Using overall dates
      x = "Ending Value of $100",
      y = "Density"
    ) +
    # Theme customization
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
      legend.position = "none", # No legend needed for single fill color
      strip.background = element_rect(fill = COLOR_AXIS_LINES_TITLE), # Background for facet titles
      strip.text = element_text(color = "white", face = "bold"), # Corrected: Use 'face' for bold
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(p) # Explicitly print the plot object
}

# --- Function to Plot Statistics Table ---
# This function generates a ggplot2-based visual table of the statistics.
plot_statistics_table <- function(stats_df, symbol_name) {
  # Prepare the data for plotting text
  # Create indices for plotting positions
  # y-coordinates: rows from top to bottom (higher y for top rows)
  # x-coordinates: columns from left to right (higher x for right columns)
  
  # Column names for the data part of the table
  data_col_names <- colnames(stats_df) # Including Horizon for table
  # No separate row names needed if Horizon is a column in stats_df
  
  # Create a long format table for plotting each cell's text
  # This makes it easier to position text based on numeric coordinates
  plot_data_values <- stats_df %>%
    mutate(plot_row_id = dplyr::row_number()) %>% # Numeric row ID for positioning data
    pivot_longer(
      cols = -c(Horizon, plot_row_id), # Pivot everything except Horizon and plot_row_id
      names_to = "Statistic",
      values_to = "text_value"
    ) %>%
    mutate(plot_col_id = as.numeric(factor(Statistic, levels = data_col_names[-1]))) # Numeric col ID for stats
  
  # Create plot_row_id for the stats_df directly for row headers
  stats_df_for_row_headers <- stats_df %>%
    mutate(plot_row_id = dplyr::row_number()) # Add a row ID for positioning
  
  # Create header labels (including Horizon)
  header_labels <- data.frame(
    plot_col_id = 0:(length(data_col_names) - 1), # Column indices for headers
    Statistic = data_col_names # The actual header names
  )
  
  # Calculate max plot dimensions for scaling
  max_plot_col_id <- max(plot_data_values$plot_col_id)
  max_plot_row_id <- max(plot_data_values$plot_row_id)
  
  
  # Create the ggplot object for the table
  p_table <- ggplot() +
    # Plot column headers
    geom_text(data = header_labels,
              aes(x = plot_col_id, y = max_plot_row_id + 1, label = Statistic), # Position above data rows
              hjust = 0.5, vjust = 0.5, size = 4, fontface = "bold", color = COLOR_AXIS_LINES_TITLE) +
    
    # Plot row headers (Horizon names from the original stats_df)
    geom_text(data = stats_df_for_row_headers,
              aes(x = -0.5, y = plot_row_id, label = Horizon), # Use pre-calculated plot_row_id
              hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold", color = COLOR_AXIS_FONT) +
    
    # Plot data values
    geom_text(data = plot_data_values,
              aes(x = plot_col_id, y = plot_row_id, label = text_value),
              hjust = 0.5, vjust = 0.5, size = 3.5, color = COLOR_AXIS_FONT) +
    
    # Set plot limits to ensure all text is visible and provide padding
    xlim(-0.75, max_plot_col_id + 0.75) + # Adjusted to provide left padding for row headers
    ylim(0.5, max_plot_row_id + 1.5) + # Adjusted to provide top padding for column headers
    
    # Add plot title
    labs(title = paste("Summary Statistics for $100 Growth of", symbol_name)) +
    
    # Apply a minimal theme to remove axes, grid lines, etc.
    theme_void() +
    theme(
      plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
      plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, face = "bold", size = 16, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") # Add some margin around the plot
    )
  
  print(p_table)
}


# --- Main Execution Block ---
# This is the starting point of the script's execution.
# It orchestrates fetching data, calculating growth, and generating plots/tables.

# Prepare an empty list to store growth data for each horizon
all_growth_data <- list()
# This list will store actual start/end dates for each horizon's data,
# to ensure the plot subtitle is accurate even if different data ranges are fetched.
actual_data_ranges <- list()

# Loop through each defined analysis horizon
for (h_years in ANALYSIS_HORIZONS_NUMERIC) { # Use numeric horizons here
  cat(paste0("\n--- Processing ", h_years, "-Year Growth ---\n"))
  
  current_growth_data <- get_growth_data(
    symbol = toupper(asset_symbol),
    start_date_str_raw = start_date_input,
    end_date_str_raw = end_date_input,
    source_type = data_source,
    horizon_years = h_years
  )
  
  if (!is.null(current_growth_data) && NROW(current_growth_data) > 0) {
    all_growth_data[[as.character(h_years)]] <- current_growth_data
    # Store the actual date range for this horizon
    actual_data_ranges[[as.character(h_years)]] <- list(
      start_date = attr(current_growth_data, "actual_start_date"),
      end_date = attr(current_growth_data, "actual_end_date")
    )
  } else {
    # Provide more specific feedback if a horizon is skipped
    message(paste0("Skipping ", h_years, "-year growth due to insufficient data or other issues. Ensure 'start_date_input' provides enough history for this horizon."))
  }
}

# Combine all successfully processed growth data into a single data frame
combined_growth_data_df <- bind_rows(all_growth_data)

# Extract the overall actual start/end dates from the collected ranges
# This will be the earliest start date and latest end date across all successful horizons.
if (length(actual_data_ranges) > 0) {
  overall_actual_start_date <- as.Date(min(sapply(actual_data_ranges, `[[`, "start_date"), na.rm = TRUE), origin = "1970-01-01")
  overall_actual_end_date <- as.Date(max(sapply(actual_data_ranges, `[[`, "end_date"), na.rm = TRUE), origin = "1970-01-01")
} else {
  overall_actual_start_date <- NA
  overall_actual_end_date <- NA
}


# --- Generate Plot and Statistics Table ---
if (!is.null(combined_growth_data_df) && NROW(combined_growth_data_df) > 0) {
  # Generate the plot
  plot_growth_distribution(
    data_df = combined_growth_data_df,
    symbol = toupper(asset_symbol),
    horizons_labels = ANALYSIS_HORIZONS_LABELS, # Pass the ordered labels for facets
    overall_start_date = overall_actual_start_date, # Pass the overall dates
    overall_end_date = overall_actual_end_date
  )
  
  # --- Calculate Statistics for Plot-Based Table ---
  cat("\n--- Preparing Statistics Table ---\n")
  
  # Group by horizon and summarize statistics
  growth_stats_table <- combined_growth_data_df %>%
    # Ensure Horizon is a factor *before* grouping for consistent ordering in output
    mutate(Horizon = factor(Horizon, levels = ANALYSIS_HORIZONS_LABELS)) %>%
    group_by(Horizon) %>% # Group by the *label* (e.g., "6-Month Growth")
    summarise(
      Q1 = quantile(as.numeric(EndingValue), probs = 0.25, na.rm = TRUE),
      Median = quantile(as.numeric(EndingValue), probs = 0.50, na.rm = TRUE),
      Q3 = quantile(as.numeric(EndingValue), probs = 0.75, na.rm = TRUE),
      IQR = IQR(as.numeric(EndingValue), na.rm = TRUE),
      Mean_Ending_Value = mean(as.numeric(EndingValue), na.rm = TRUE),
      Std_Dev_Ending_Value = sd(as.numeric(EndingValue), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Calculate Average CAGR from Mean Ending Value (for each horizon)
    # Extract the numeric year from the Horizon label for CAGR calculation
    mutate(HorizonYears = case_when(
      Horizon == "6-Month Growth" ~ 0.5,
      # Use parse_number for more robust numeric extraction
      TRUE ~ readr::parse_number(as.character(Horizon))
    )) %>%
    # Add a check for NA HorizonYears to prevent issues in CAGR calculation
    filter(!is.na(HorizonYears) & HorizonYears > 0) %>% # Ensure HorizonYears is valid and not zero
    mutate(Avg_CAGR = (Mean_Ending_Value / 100)^(1 / HorizonYears) - 1) %>%
    # Reorder columns to ensure 'Horizon' is first
    select(Horizon, Q1, Median, Q3, IQR, `Avg Ending Value` = Mean_Ending_Value,
           `Std Dev Ending Value` = Std_Dev_Ending_Value, `Avg CAGR` = Avg_CAGR) %>%
    # Ensure the table is sorted by horizon for consistent display
    arrange(match(Horizon, ANALYSIS_HORIZONS_LABELS))
  
  
  # Format columns for readability in the table plot
  growth_stats_table_formatted <- growth_stats_table %>%
    mutate(
      Q1 = scales::dollar(Q1, accuracy = 0.01),
      Median = scales::dollar(Median, accuracy = 0.01),
      Q3 = scales::dollar(Q3, accuracy = 0.01),
      IQR = scales::dollar(IQR, accuracy = 0.01),
      `Avg Ending Value` = scales::dollar(`Avg Ending Value`, accuracy = 0.01),
      `Std Dev Ending Value` = scales::dollar(`Std Dev Ending Value`, accuracy = 0.01),
      `Avg CAGR` = scales::percent(`Avg CAGR`, accuracy = 0.01)
    )
  
  # Generate the statistics table plot
  plot_statistics_table(
    stats_df = growth_stats_table_formatted,
    symbol_name = toupper(asset_symbol)
  )
  
} else {
  # If data processing failed for all horizons, inform the user.
  message("No growth data could be processed for any horizon. Plot and table cannot be generated.")
  message("Please review the messages above for specific errors (e.g., symbol validity, date range, data source).")
}