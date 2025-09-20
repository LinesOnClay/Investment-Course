# --- USER INPUTS & CUSTOMIZATION
# --------------------------------------------------------------------------
# --- MODIFY THESE VALUES ---

# Set the start and end dates for the analysis.
# IMPORTANT: Use "MM-DD-YYYY" or "YYYY-MM-DD" format for start_date_str
# Example: "01-01-2001" for January 1, 2001.
start_date_str <- "01-01-2012" # User's requested start date
end_date <- "05-01-2014"

# Enter any FRED or Yahoo Finance symbols here. UP TO SIX.
symbols_to_get <- c("SPY","PCM","AGG","LQD") # From Yahoo Finance


# --- Manually define full names for FRED symbols ONLY ---
# (Currently none in symbols_to_get, but good to keep for future use)
symbol_names <- c(
  "BAMLCC0A1AAATRIV" = "ICE BofA US Corp. Index",
  "BAMLHYH0A0HYM2TRIV" = "ICE BofA US High Yield Index"
)

#________________________________________________________________


# --- Install and Load Necessary Packages ---
# These lines check if packages are installed and install them if they're not.
# They will install automatically if needed, then load the libraries.
if (!requireNamespace("tidyquant", quietly = TRUE)) {
  install.packages("tidyquant")
}
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("ggtext", quietly = TRUE)) { # Re-enabling ggtext for rich text in ggplot
  install.packages("ggtext")
}

library(tidyquant)
library(quantmod)    # Needed for getQuote to fetch names
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)   # For easier date parsing (mdy, dmy, ymd)
library(ggtext)      # For the rich text summary box



# --- Helper function to get names for Yahoo tickers ---
get_yahoo_name <- function(ticker) {
  cat(paste("Fetching name for", ticker, "...\n"))
  tryCatch({
    # Use suppressWarnings because getQuote can sometimes throw warnings for minor issues
    name <- suppressWarnings(quantmod::getQuote(ticker, what = quantmod::yahooQF("Name"))$Name)
    if (!is.na(name) && nchar(name) > 0) return(name) else return(ticker)
  }, error = function(e) {
    warning(paste("Could not fetch name for", ticker, "- using ticker symbol as name. Error:", e$message), call. = FALSE)
    return(ticker)
  })
}

# --- Automatically populate names for Yahoo symbols ---
yahoo_symbols_to_name <- symbols_to_get[!symbols_to_get %in% names(symbol_names)]
if (length(yahoo_symbols_to_name) > 0) {
  dynamic_names <- sapply(yahoo_symbols_to_name, get_yahoo_name)
  symbol_names <- c(symbol_names, dynamic_names)
}

# --- PLOT AESTHETICS ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
PALETTE_MAIN <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318", "#6a4c93")


# 3. DATA RETRIEVAL & CALCULATION
# --------------------------------------------------------------------------
cat("Downloading and calculating cumulative growth...\n")

# Convert start_date_str to a proper Date object
# Using mdy() as per "MM-DD-YYYY" example. If "YYYY-MM-DD" is used, change to ymd().
start_date_obj <- mdy(start_date_str)

# Fetch data for all symbols
all_data <- tq_get(
  symbols_to_get,
  get = "stock.prices",
  from = start_date_obj, # Request data from user's start date
  to = end_date
)

# Process fetched data: select relevant columns, handle NA/zero prices
all_data <- all_data %>%
  mutate(price = ifelse(is.na(adjusted), close, adjusted)) %>%
  select(symbol, date, price) %>%
  filter(!is.na(price) & price > 0) # CRITICAL: Ensure price is not NA and is positive

# --- Debug: Check all_data after initial fetching and cleaning ---
cat("\n--- DEBUG: all_data after initial fetch and cleaning ---\n")
print(paste("Number of rows:", NROW(all_data)))
print(head(all_data))
print(tail(all_data))
cat("---\n")


# --- CRITICAL FIX: Filter data to exact start date *after* fetching ---
# This ensures that all calculations start precisely from the user's desired date,
# even if tq_get fetched earlier data for some symbols (e.g., if a security
# existed before the user's start_date_obj).
all_data_filtered <- all_data %>%
  filter(date >= start_date_obj)

# Handle cases where a symbol might have no data after filtering
if (NROW(all_data_filtered) == 0) {
  stop("No data available for the specified symbols after filtering by the requested start date. Please check symbol validity or adjust the start date.")
}

# --- Debug: Check all_data_filtered ---
cat("\n--- DEBUG: all_data_filtered after date filtering ---\n")
print(paste("Number of rows:", NROW(all_data_filtered)))
print(head(all_data_filtered))
print(tail(all_data_filtered))
print(all_data_filtered %>% group_by(symbol) %>% summarise(min_date_filtered = min(date), max_date_filtered = max(date), n_rows = n()))
cat("---\n")


# Determine the actual earliest date available across all *filtered* symbols
# This will be the earliest date used in the plot for any of the securities.
actual_start_date <- min(all_data_filtered$date)
cat(paste("User-defined start date:", format(start_date_obj, "%Y-%m-%d"), "\n"))
cat(paste("Actual start date based on available data after filtering:", format(actual_start_date, "%Y-%m-%d"), "\n"))


# Calculate cumulative growth for each symbol
cumulative_growth_data <- all_data_filtered %>% # Use the filtered data frame here
  group_by(symbol) %>%
  arrange(date) %>%
  # Get the initial price for each symbol in the group
  mutate(initial_price = first(price)) %>%
  # Calculate Growth, explicitly handling potential division by zero or NA initial price
  mutate(Growth = 100 * price / initial_price) %>%
  ungroup() %>%
  # CRITICAL: Filter out any rows where Growth became NaN/Inf/NA due to data issues
  filter(is.finite(Growth))

# --- Debug: Check cumulative_growth_data ---
cat("\n--- DEBUG: cumulative_growth_data after growth calculation ---\n")
print(paste("Number of rows:", NROW(cumulative_growth_data)))
print(str(cumulative_growth_data))
print(summary(cumulative_growth_data$Growth))
print(head(cumulative_growth_data))
print(cumulative_growth_data %>% group_by(symbol) %>% summarise(min_date_growth = min(date), max_date_growth = max(date), n_rows = n(), min_growth = min(Growth), max_growth = max(Growth)))
cat("---\n")


# 4. PREPARE DATA FOR PLOTTING
# --------------------------------------------------------------------------
# Determine the order for the legend and summary box based on final wealth
summary_box_data <- cumulative_growth_data %>%
  group_by(symbol) %>%
  filter(date == max(date)) %>%
  ungroup() %>% # Corrected typo from ung() to ungroup()
  arrange(desc(Growth))

ordered_levels <- summary_box_data$symbol

# Apply the new order to the main data
cumulative_growth_data$symbol <- factor(cumulative_growth_data$symbol, levels = ordered_levels)

cat("Data prepared for plotting.\n")


# 5. CREATE THE LINE GRAPH VISUALIZATION (ggplot2)
# --------------------------------------------------------------------------
# --- Create color and label mappings ---
color_mapping <- setNames(PALETTE_MAIN[1:length(ordered_levels)], ordered_levels)
label_mapping <- setNames(symbol_names[ordered_levels], ordered_levels)

# --- Create the rich text annotation for the top-left corner ---
annotation_text <- "<strong>Closing Wealth Amounts</strong><br>"
for (i in 1:nrow(summary_box_data)) {
  ticker <- summary_box_data$symbol[i]
  full_name <- symbol_names[ticker]
  value <- summary_box_data$Growth[i]
  color <- color_mapping[ticker]
  
  annotation_text <- paste0(
    annotation_text,
    "<span style='color:", color, ";'><strong>", full_name, " (", ticker, "):</strong> ", dollar(value, accuracy = 0.01), "</span><br>"
  )
}

# Build the plot
cumulative_line_plot <- ggplot(cumulative_growth_data, aes(x = date, y = Growth, color = symbol)) +
  geom_line(size = 1) +
  # --- Add the annotation box ---
  annotate(
    "richtext", # Use richtext for ggtext
    x = actual_start_date, # Use the correctly filtered actual_start_date
    y = Inf, # Position at the top
    label = annotation_text,
    hjust = 0, vjust = 1, # Align to top-left
    size = 4.5,
    lineheight = 1.3,
    label.color = NA, # No border for the label
    fill = alpha(COLOR_BACKGROUND, 0.6) # Semi-transparent fill
  ) +
  scale_color_manual(
    values = color_mapping,
    labels = label_mapping
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  # Adjust x-axis breaks if needed, considering the new, potentially shorter date range
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0.01, 0.01)) +
  labs(
    title = "Cumulative Growth of a $100 Investment",
    subtitle = paste("Performance from", format(actual_start_date, "%b %Y"), "to", format(end_date, "%b %Y")),
    x = NULL, y = "Investment Value"
  ) +
  guides(color = guide_legend(title = "Asset", title.position = "top", title.hjust = 0.5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.grid.major = element_line(color = "#d9d9d9", linetype = "solid", size = 0.4),
    panel.grid.minor = element_line(color = "#e9e9e9", linetype = "dotted", size = 0.2),
    plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 20, face = "bold"),
    plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 14, margin = margin(b = 15)),
    axis.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 12, face = "bold"),
    axis.text = element_text(color = COLOR_AXIS_FONT, size = 10),
    legend.title = element_text(color = COLOR_AXIS_LINES_TITLE, face = "bold"),
    legend.text = element_text(color = COLOR_AXIS_FONT),
    legend.position = "top",
    legend.box.just = "center",
    axis.line = element_line(color = COLOR_AXIS_FONT),
    axis.ticks = element_line(color = COLOR_AXIS_FONT),
    plot.margin = margin(5.5, 15, 5.5, 5.5, "pt")
  )

# 6. DISPLAY THE LINE GRAPH
# --------------------------------------------------------------------------
print(cumulative_line_plot)
cat("\n--- Line Graph Generated Successfully ---\n")