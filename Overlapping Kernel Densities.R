# ---
# Title: Investment Return Distribution Analysis (Kernel Density)
# Author: Gemini
# Date: 2024-06-28
# Description: This script downloads historical price data for up to six financial
#              assets (stocks, ETFs, etc.), calculates their rolling one-year
#              total returns, and visualizes the distribution of these returns
#              as overlapping kernel density plots using ggplot2.
# ---

# 1. SETUP: LOAD NECESSARY LIBRARIES
# --------------------------------------------------------------------------
# We need a few packages for this script:
# - quantmod: For downloading financial data from sources like Yahoo Finance.
# - ggplot2: For creating beautiful and customizable plots.
# - dplyr: For data manipulation and cleaning.
# - tidyr: For tidying data, specifically to reshape it for plotting.
# - xts: To work with extensible time series objects.

# If you don't have these packages installed, you can install them by running:
# install.packages(c("quantmod", "ggplot2", "dplyr", "tidyr", "xts"))

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)

# 2. USER INPUTS & CUSTOMIZATION
# --------------------------------------------------------------------------
# --- MODIFY THESE VALUES ---

# Enter up to six ticker symbols. Examples: "SPY" (S&P 500 ETF), "AGG" (Bond ETF),
# "GLD" (Gold ETF), "AAPL" (Apple Inc.), "MSFT" (Microsoft Corp).
# For FRED data, you can use series IDs like "DGS10" for the 10-Year Treasury Yield.
symbols <- c("AGG", "SHY", "LQD", "JNK")

# Set the start and end dates for the analysis.
# The format is "YYYY-MM-DD".
start_date <- "1997-01-01"
end_date <- Sys.Date() # Defaults to today's date.

# Specify the data source. Common options are "yahoo" for Yahoo Finance
# or "FRED" for the Federal Reserve Economic Data.
data_source <- "yahoo"

# --- PLOT AESTHETICS ---
# These are the custom colors specified for the plot's aesthetics.
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"

# We also need a color palette for the plots themselves.
# Here's a palette that works well with the theme colors.
PALETTE_DENSITY <- c("#0077b6", "#f94144", "#90be6d", "#f8961e", "#43aa8b", "#577590")


# 3. DATA RETRIEVAL & PREPARATION
# --------------------------------------------------------------------------
# This section fetches the data and prepares it for analysis.

# Create an empty environment to store the downloaded data.
data_env <- new.env()

# Download the data for each symbol using getSymbols from the quantmod package.
# The `auto.assign = TRUE` argument places each symbol's data into our environment.
cat("Downloading data...\n")
suppressWarnings( # Suppress warnings about NA coercion during download
  getSymbols(
    Symbols = symbols,
    src = data_source,
    from = start_date,
    to = end_date,
    auto.assign = TRUE,
    env = data_env
  )
)

# We are interested in the "Adjusted Close" price, which accounts for dividends
# and stock splits. This is the best measure for calculating total return.
# We'll extract the adjusted close column from each downloaded dataset.
adjusted_prices_list <- lapply(ls(data_env), function(x) Ad(get(x, envir = data_env)))
names(adjusted_prices_list) <- ls(data_env)


# Merge all the individual time series into a single object.
# The `merge.xts` function is smart: it aligns all series by date and
# automatically fills in `NA` for dates where a particular series has no data.
merged_prices <- do.call(merge.xts, adjusted_prices_list)
colnames(merged_prices) <- gsub("\\.Adjusted", "", colnames(merged_prices))

cat("Data download and merge complete.\n")


# 4. CALCULATE ROLLING 1-YEAR GROWTH OF $100
# --------------------------------------------------------------------------
# This section uses a stable method with the `dplyr` package to calculate returns.
# It directly calculates the ratio of the price to the price from 252 trading days ago.

cat("Calculating rolling returns...\n")

# A one-year period is approximately 252 trading days.
trading_days_in_year <- 252

# Calculate the growth of $100 for each asset.
# The formula is: 100 * (Price Today / Price 252 days ago)
# We use the `lag()` function to get the price from 252 days ago.
rolling_growth <- 100 * (merged_prices / lag.xts(merged_prices, k = trading_days_in_year))
colnames(rolling_growth) <- colnames(merged_prices)


# 5. TIDY DATA FOR PLOTTING
# --------------------------------------------------------------------------
# `ggplot2` works best with data in a "long" format, where each row is a
# single observation. We need to convert our wide `rolling_growth` object.

# First, convert the xts object to a standard data frame.
growth_df <- as.data.frame(rolling_growth)
growth_df$Date <- index(rolling_growth)

# Now, use `pivot_longer` from `tidyr` to reshape the data.
# We transform it from columns of symbols to two columns:
# - 'Ticker': The name of the symbol.
# - 'Growth': The calculated growth of $100 for that day.
growth_long_df <- growth_df %>%
  pivot_longer(
    cols = -Date,
    names_to = "Ticker",
    values_to = "Growth"
  ) %>%
  na.omit() # Remove all rows with NA values (e.g., the first year of data).

cat("Data prepared for plotting.\n")


# 6. CREATE THE GGPLOT VISUALIZATION (KERNEL DENSITY)
# --------------------------------------------------------------------------
# Now we use ggplot2 to build our final plot using kernel density estimates.

# Ensure the 'Ticker' column is a factor to control plotting order and colors.
# We check which symbols actually have data before setting the factor levels.
available_symbols <- intersect(symbols, unique(growth_long_df$Ticker))
growth_long_df$Ticker <- factor(growth_long_df$Ticker, levels = available_symbols)

# Build the plot layer by layer.
return_density_plot <- ggplot(growth_long_df, aes(x = Growth, fill = Ticker, color = Ticker)) +
  
  # Add the kernel density geometry.
  # `alpha` makes the filled areas semi-transparent. Here it's 75% transparent.
  # `size` controls the thickness of the outline.
  geom_density(alpha = 0.25, size = 1) +
  
  # Apply our custom color palette for the fills and the lines.
  scale_fill_manual(values = PALETTE_DENSITY) +
  scale_color_manual(values = PALETTE_DENSITY) +
  
  # Add a vertical line at $100, which represents the break-even point (no growth).
  geom_vline(xintercept = 100, linetype = "dashed", color = COLOR_AXIS_FONT, size = 1) +
  
  # Set plot labels and title.
  labs(
    title = "Distribution of 1-Year Investment Growth",
    subtitle = paste("Based on rolling 252-day periods from", format(min(growth_long_df$Date), "%b %Y"), "to", format(max(growth_long_df$Date), "%b %Y")),
    x = "Value of $100 After One Year",
    y = "Density", # Y-axis is now Density, not Frequency
    fill = "Asset",
    color = "Asset" # Also label the color legend
  ) +
  
  # Customize the plot's theme (colors, fonts, lines).
  theme_minimal() +
  theme(
    # Plot background and panel background
    plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "#e0e0e0", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    # Text elements (title, subtitle, axis labels)
    plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 20, face = "bold"),
    plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 14, margin = margin(b = 15)),
    axis.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 12, face = "bold"),
    axis.text = element_text(color = COLOR_AXIS_FONT, size = 10),
    legend.title = element_text(color = COLOR_AXIS_LINES_TITLE, face = "bold"),
    legend.text = element_text(color = COLOR_AXIS_FONT),
    
    # Axis lines
    axis.line = element_line(color = COLOR_AXIS_FONT),
    axis.ticks = element_line(color = COLOR_AXIS_FONT)
  )

# 7. DISPLAY THE PLOT
# --------------------------------------------------------------------------
print(return_density_plot)

cat("Density plot generated successfully!\n")
