# USER INPUTS & CUSTOMIZATION
# --------------------------------------------------------------------------
# --- MODIFY THESE VALUES ---

# Set the start and end dates for the analysis.
# IMPORTANT: Use "MM-DD-YYYY" or "YYYY-MM-DD" format for both dates.
# `lubridate` functions like `mdy()` or `ymd()` will parse based on this.
start_date_str <- "3-1-2024" #enter start date within the quotes
end_date_str <- "7-22-2025" #enter end date within the quotes

# Enter any FRED or Yahoo Finance symbols here. UP TO SIX.
symbols_to_get <- c("VUG", "RSP","VTV") # From Yahoo Finance


# --- Manually define full names for FRED symbols ONLY ---
# If you have FRED symbols in `symbols_to_get`, add their full names here.
# Example: "BAMLCC0A1AAATRIV" = "ICE BofA US Corp. Index"
manual_symbol_names <- c(
  "BAMLCC0A1AAATRIV" = "ICE BofA US Corp. Index",
  "BAMLHYH0A0HYM2TRIV" = "ICE BofA US High Yield Index"
)

# --- PLOT AESTHETICS ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"
PALETTE_MAIN <- c("#2a6ca3", "#e8c547", "#8fb8de", "#f7b076", "#333f48", "#DC5318", "#6a4c93")


# 1. SETUP: LOAD NECESSARY LIBRARIES
# --------------------------------------------------------------------------
# If you don't have these packages installed, you can install them by running:
# install.packages(c("tidyquant", "quantmod", "ggplot2", "dplyr", "scales", "lubridate", "ggtext"))

library(tidyquant)
library(quantmod)    # Needed for getQuote to fetch names
library(ggplot2)
library(dplyr)       # For data manipulation
library(scales)
library(lubridate)   # For easier date parsing (mdy, dmy, ymd)
library(ggtext)      # For the rich text summary box


# 2. HELPER FUNCTIONS & DYNAMIC NAME POPULATION
# --------------------------------------------------------------------------

# --- Helper function to get names for Yahoo tickers ---
get_yahoo_name <- function(ticker) {
  cat(paste("Fetching name for", ticker, "...\n"))
  tryCatch({
    name <- quantmod::getQuote(ticker, what = quantmod::yahooQF("Name"))$Name
    if (!is.na(name) && nchar(name) > 0) return(name) else return(ticker)
  }, error = function(e) {
    warning(paste("Could not fetch name for", ticker, "- using ticker symbol as name."), call. = FALSE)
    return(ticker)
  })
}

# --- Automatically populate names for Yahoo symbols ---
symbol_names <- manual_symbol_names # Start with manually defined names

yahoo_symbols_to_name <- symbols_to_get[!symbols_to_get %in% names(symbol_names)]
if (length(yahoo_symbols_to_name) > 0) {
  dynamic_names <- sapply(yahoo_symbols_to_name, get_yahoo_name)
  symbol_names <- c(symbol_names, dynamic_names)
}


# 3. DATA RETRIEVAL & CALCULATION
# --------------------------------------------------------------------------
cat("Downloading and calculating cumulative growth...\n")

# Convert start_date_str and end_date_str to proper Date objects
# Use mdy() for "MM-DD-YYYY" or ymd() for "YYYY-MM-DD"
start_date_obj <- mdy(start_date_str)
end_date_obj <- mdy(end_date_str) # Using mdy() as per your example "05-01-2014"

# --- Validate Date Order ---
if (start_date_obj >= end_date_obj) {
  stop("Error: The 'start_date_str' must be earlier than the 'end_date_str'. Please adjust your dates.")
}


all_data <- tq_get(
  symbols_to_get,
  get = "stock.prices",
  from = start_date_obj, # Pass the Date object
  to = end_date_obj     # Pass the Date object
) %>%
  mutate(price = ifelse(is.na(adjusted), close, adjusted)) %>%
  select(symbol, date, price)

# --- IMPORTANT FIX: Filter data to exact start date *before* calculations ---
# This ensures that calculations only use data from the desired start date onwards
all_data_filtered <- all_data %>%
  filter(date >= start_date_obj & date <= end_date_obj)


# Determine the actual earliest date available across all *filtered* symbols
# This will now correctly reflect the user's intended start date (or later if a ticker starts later)
actual_start_date <- min(all_data_filtered$date)
actual_end_date <- max(all_data_filtered$date) # Get actual end date
cat(paste("User-defined start date:", format(start_date_obj, "%Y-%m-%d"), "\n"))
cat(paste("User-defined end date:", format(end_date_obj, "%Y-%m-%d"), "\n"))
cat(paste("Actual start date based on available data after filtering:", format(actual_start_date, "%Y-%m-%d"), "\n"))
cat(paste("Actual end date based on available data after filtering:", format(actual_end_date, "%Y-%m-%d"), "\n"))


cumulative_growth_data <- all_data_filtered %>% # Use the filtered data frame here
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(Growth = 100 * price / first(price)) %>%
  ungroup()


# 4. PREPARE DATA FOR PLOTTING
# --------------------------------------------------------------------------
# Determine the order for the legend and summary box based on final wealth
summary_box_data <- cumulative_growth_data %>%
  group_by(symbol) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  arrange(desc(Growth))

ordered_levels <- summary_box_data$symbol

# Apply the new order to the main data
cumulative_growth_data$symbol <- factor(cumulative_growth_data$symbol, levels = ordered_levels)

cat("Data prepared for plotting.\n")


# 5. CREATE THE LINE GRAPH VISUALIZATION
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
  
  # --- UPDATED: New text format ---
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
    "richtext",
    x = actual_start_date, # Use the correctly filtered actual_start_date
    y = Inf,
    label = annotation_text,
    hjust = 0, vjust = 1,
    # --- UPDATED: Larger text size ---
    size = 4.5,
    lineheight = 1.3,
    label.color = NA,
    fill = alpha(COLOR_BACKGROUND, 0.6)
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
    subtitle = paste("Performance from", format(actual_start_date, "%b %Y"), "to", format(actual_end_date, "%b %Y")), # Use actual_end_date here
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