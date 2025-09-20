# ---
# Title: Investment Return Distribution Analysis (Final Polished Version)
# Date: 2025-06-30
# Description: This script downloads historical price data, automatically fetches
#              the full name for each ticker, calculates rolling one-year total
#              returns, and visualizes the distribution as violin plots ordered
#              by volatility, with full descriptive labels and summary statistics.
# ---
# 1. USER INPUTS & CUSTOMIZATION (AUTO-NAMING)
# --------------------------------------------------------------------------
# --- MODIFY THESE VALUES ---

# You can use one, both, or neither list. The script will adapt.
# To skip a source, just leave its list empty: e.g., fred_symbols <- c()


# Set the start and end dates for the analysis.
start_date <- "2010-01-01"
end_date <- Sys.Date()


# Enter any standard stock/ETF ticker symbols here.
# The script will now AUTOMATICALLY find the full name for these tickers.
yahoo_symbols <- c("SPY","RSP","VTV", "VUG", "EQL")

# Enter any FRED data series symbols here.  For example, c("BAMLCC0A1AAATRIV","BAMLHYH0A0HYM2TRIV")
fred_symbols <- c()





#______________________________________________________

# 2. SETUP: LOAD NECESSARY LIBRARIES
# --------------------------------------------------------------------------
# We need a few packages for this script:
# - quantmod: For downloading financial data.
# - ggplot2: For creating beautiful and customizable plots.
# - dplyr: For data manipulation and cleaning.
# - tidyr: For tidying data, specifically to reshape it for plotting.
# - xts: To work with extensible time series objects.
# - scales: For formatting axis labels (e.g., adding dollar signs).
# - ggtext: For advanced text formatting in plots (e.g., bolding).

# If you don't have these packages installed, you can install them by running:
# install.packages(c("quantmod", "ggplot2", "dplyr", "tidyr", "xts", "scales", "ggtext"))

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
library(scales)
library(ggtext)



# Combine the lists for later use.
all_symbols <- c(fred_symbols, yahoo_symbols)

# --- Manually define full names for FRED symbols ONLY ---
# Yahoo symbols are handled automatically below.
symbol_names <- c(
  "BAMLCC0A1AAATRIV" = "ICE BofA US Corp. Index",
  "BAMLHYH0A0HYM2TRIV" = "ICE BofA US High Yield Index"
)

# --- Helper function to get names for Yahoo tickers ---
get_yahoo_name <- function(ticker) {
  cat(paste("Fetching name for", ticker, "...\n"))
  tryCatch({
    name <- quantmod::getQuote(ticker, what = quantmod::yahooQF("Name"))$Name
    # Return the name if it's not empty or NA, otherwise return the ticker
    if (!is.na(name) && nchar(name) > 0) return(name) else return(ticker)
  }, error = function(e) {
    # In case of any error (e.g., ticker not found), return the ticker itself
    warning(paste("Could not fetch name for", ticker, "- using ticker symbol as name."), call. = FALSE)
    return(ticker)
  })
}

# --- Automatically populate names for Yahoo symbols ---
if (length(yahoo_symbols) > 0) {
  dynamic_names <- sapply(yahoo_symbols, get_yahoo_name)
  # Add the dynamically fetched names to our manual list
  symbol_names <- c(symbol_names, dynamic_names)
}


# --- PLOT AESTHETICS ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
PALETTE_VIOLIN <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318")


# 3. DATA RETRIEVAL & PREPARATION (MULTI-SOURCE LOGIC)
# --------------------------------------------------------------------------
data_env <- new.env()
if (length(fred_symbols) > 0) {
  cat("Downloading data from FRED...\n")
  suppressWarnings(getSymbols(fred_symbols, src = "FRED", from = start_date, to = end_date, auto.assign = TRUE, env = data_env))
}
if (length(yahoo_symbols) > 0) {
  cat("Downloading data from Yahoo Finance...\n")
  suppressWarnings(getSymbols(yahoo_symbols, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE, env = data_env))
}

if (length(ls(data_env)) == 0) {
  stop("No symbols were provided or no data could be downloaded. Please check your symbol lists.")
}

adjusted_prices_list <- lapply(ls(data_env), function(x) {
  data_object <- get(x, envir = data_env)
  if (is.OHLC(data_object)) Ad(data_object) else data_object
})
names(adjusted_prices_list) <- ls(data_env)
merged_prices <- do.call(merge.xts, adjusted_prices_list)
colnames(merged_prices) <- gsub("\\.Adjusted", "", colnames(merged_prices))
cat("All data downloaded and merged successfully.\n")


# 4. CALCULATE ROLLING 1-YEAR GROWTH OF $100
# --------------------------------------------------------------------------
cat("Calculating rolling returns...\n")
trading_days_in_year <- 252
rolling_growth <- 100 * (merged_prices / lag.xts(merged_prices, k = trading_days_in_year))
colnames(rolling_growth) <- colnames(merged_prices)


# 5. TIDY DATA FOR PLOTTING
# --------------------------------------------------------------------------
growth_df <- as.data.frame(rolling_growth)
growth_df$Date <- index(rolling_growth)
growth_long_df <- growth_df %>%
  pivot_longer(cols = -Date, names_to = "Ticker", values_to = "Growth") %>%
  na.omit()
cat("Data prepared for plotting.\n")


# 6. CREATE THE GGPLOT VISUALIZATION (FINAL)
# --------------------------------------------------------------------------
available_symbols <- intersect(all_symbols, unique(growth_long_df$Ticker))
n_symbols <- length(available_symbols)

summary_data <- growth_long_df %>%
  group_by(Ticker) %>%
  summarise(
    mean_growth = mean(Growth, na.rm = TRUE),
    sd_growth = sd(Growth, na.rm = TRUE),
    text_y_pos = max(Growth, na.rm = TRUE) * 1.05,
    .groups = 'drop'
  ) %>%
  arrange(sd_growth) %>%
  mutate(
    label = paste0("Avg: ", dollar(mean_growth, accuracy = 0.1), "\nSD: ", dollar(sd_growth, accuracy = 0.1)),
    x_pos = row_number()
  )

ordered_levels <- summary_data$Ticker
growth_long_df$Ticker <- factor(growth_long_df$Ticker, levels = ordered_levels)
growth_long_df <- growth_long_df %>%
  left_join(select(summary_data, Ticker, x_pos), by = "Ticker")

outlier_data <- growth_long_df %>%
  group_by(Ticker) %>%
  mutate(
    lower_bound = quantile(Growth, 0.02, na.rm = TRUE),
    upper_bound = quantile(Growth, 0.98, na.rm = TRUE)
  ) %>%
  filter(Growth < lower_bound | Growth > upper_bound) %>%
  slice_sample(prop = 1/3) %>%
  ungroup()

y_breaks <- sort(unique(c(100, pretty(growth_long_df$Growth, n = 8))))
repeated_axis_data <- if (n_symbols > 2) {
  axis_positions <- seq(2.5, n_symbols, by = 2)
  expand.grid(x = axis_positions, y = y_breaks)
} else { data.frame() }

plot_labels <- paste0(
  ordered_levels,
  "<br>**",
  ifelse(is.na(symbol_names[ordered_levels]), ordered_levels, symbol_names[ordered_levels]),
  "**"
)

return_violin_plot <- ggplot(growth_long_df, aes(x = x_pos, y = Growth, fill = Ticker, group = Ticker)) +
  # --- UPDATED: Changed linetype to "dotted" ---
  geom_segment(aes(x = 0.5, xend = n_symbols + 0.5, y = 100, yend = 100), linetype = "dotted", color = "#DC5318", size = 0.6) +
  geom_violin(draw_quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), trim = FALSE, alpha = 0.7, color = "white", size = 0.5) +
  geom_jitter(data = outlier_data, aes(color = Ticker), width = 0.1, size = 1.5, alpha = 0.8, show.legend = FALSE) +
  geom_text(data = summary_data, aes(y = text_y_pos, label = label), size = 3.5, color = COLOR_AXIS_FONT, vjust = 0, lineheight = 0.9, fontface = "bold") +
  scale_fill_manual(values = PALETTE_VIOLIN, guide = "none") +
  scale_color_manual(values = PALETTE_VIOLIN, guide = "none") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = y_breaks) +
  scale_x_continuous(breaks = 1:n_symbols, labels = plot_labels) +
  labs(
    title = "Distribution of One-Year Investment Growth",
    subtitle = paste("Based on rolling 252-day periods from", format(min(growth_long_df$Date), "%b %Y"), "to", format(max(growth_long_df$Date), "%b %Y")),
    x = NULL, y = "Value of $100 After One Year",
    caption = "Violin lines represent 10th, 25th, 50th, 75th, and 90th percentiles."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d9d9d9", linetype = "solid", size = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 20, face = "bold"),
    plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 14, margin = margin(b = 15)),
    axis.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 12, face = "bold"),
    axis.text.x = element_markdown(color = COLOR_AXIS_FONT, size = 9, lineheight = 1.1),
    axis.text.y = element_text(color = COLOR_AXIS_FONT, size = 10),
    legend.position = "none",
    axis.line = element_line(color = COLOR_AXIS_FONT),
    axis.ticks = element_line(color = COLOR_AXIS_FONT),
    plot.margin = margin(15, 15, 5.5, 5.5, "pt"),
    plot.caption = element_text(hjust = 1, size = 9, color = COLOR_AXIS_LINES_TITLE, margin = margin(t = 10))
  )

if (nrow(repeated_axis_data) > 0) {
  return_violin_plot <- return_violin_plot +
    geom_segment(
      data = data.frame(x = repeated_axis_data$x, y_min = min(y_breaks), y_max = max(y_breaks)),
      aes(x = x, xend = x, y = y_min, yend = y_max),
      inherit.aes = FALSE, color = COLOR_AXIS_FONT, size = 0.5
    ) +
    geom_text(
      data = repeated_axis_data,
      aes(x = x, y = y, label = dollar(y)),
      inherit.aes = FALSE, hjust = -0.15, size = 3.5, color = COLOR_AXIS_FONT
    ) +
    coord_cartesian(clip = "off")
}

# 7. DISPLAY THE PLOT
# --------------------------------------------------------------------------
print(return_violin_plot)

cat("Final violin plot generated successfully!\n")
