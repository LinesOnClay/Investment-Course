# --- 1. Install and Load Necessary R Packages ---
if (!requireNamespace("googlesheets4", quietly = TRUE)) install.packages("googlesheets4")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales") # For percentage formatting
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats") # For factor reordering

library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(forcats) # Load forcats explicitly for fct_relevel

# --- 2. Define Google Sheet Details ---
spreadsheet_url <- "https://docs.google.com/spreadsheets/d/1rzJmTcmxVKjqi7SgzjGuU4RxVzK9prHX7ojE3I-UZVQ/edit?usp=sharing"
sheet_name <- "Returns by year1" # Using "Returns by year1" tab
range_to_read <- "A:H" # Reading all columns A through H, headers in Row 1

# --- 3. Robust Authentication with Google Sheets ---
message("Attempting to de-authorize any existing Google Sheets tokens...")
tryCatch({
  gs4_deauth()
  message("Existing tokens de-authorized (if any).")
}, error = function(e) {
  message("No existing tokens to de-authorize or error during de-authorization: ", e$message)
})

message("Attempting to authenticate with Google Sheets...")
tryCatch({
  read_sheet(ss = spreadsheet_url, sheet = sheet_name, range = "A1", col_names = FALSE, n_max = 1)
  message("Google Sheets authentication successful!")
}, error = function(e) {
  message("Error during Google Sheets authentication or initial read: ", e$message)
  message("Please ensure you completed the browser authentication successfully.")
  message("If prompted in the R console with 'Enter a number between 1 and 2, or enter 0 to exit.', try entering '1' and pressing Enter.")
  stop("Authentication failed. Please check your internet connection and Google account permissions.")
})

# --- 4. Read Data from Google Sheet ---
message("Reading full data from Google Sheet...")
returns_data_raw <- read_sheet(
  ss = spreadsheet_url,
  sheet = sheet_name,
  range = range_to_read,
  col_names = TRUE # Use Row 1 as column headers
)
message("Raw data successfully read from Google Sheet.")

# --- DEBUGGING: Inspect the raw data structure (for verification) ---
message("Structure of raw data after initial read:")
print(str(returns_data_raw))
message("Column names of raw data after initial read (before renaming):")
print(names(returns_data_raw))
message("First few rows of raw data:")
print(head(returns_data_raw))

# --- 5. Force-Rename Columns and Data Cleaning/Transformation ---
message("Force-renaming columns and transforming data...")

# Define the EXACT names we want for the 7 asset columns (B through H).
# These are the names that will be used in the plot and for color mapping.
# Column A is 'Year', so we target columns 2 through 8.
# The order here reflects the original column order B, C, D, E, F, G, H
# from the Google Sheet.
asset_names_in_sheet_order <- c(
  "S&P 500 Total Return",
  "US Small Cap Total Return",
  "3-mo US Treasury Bill",
  "10-yr US Treasury Bond",
  "Baa Corporate Bonds",
  "Real Estate Price Only",
  "Gold"
)

# Create a new data frame with explicitly renamed columns.
# We select by position and then assign the desired names.
returns_data_renamed <- returns_data_raw %>%
  select(1, 2, 3, 4, 5, 6, 7, 8) # Select Year (1) and then columns B-H (2-8)
names(returns_data_renamed) <- c("Year", asset_names_in_sheet_order)

message("Columns renamed to:")
print(names(returns_data_renamed))
message("First few rows after renaming:")
print(head(returns_data_renamed))


# Identify columns that are NOT 'Year' and should be numeric.
numeric_cols <- names(returns_data_renamed)[names(returns_data_renamed) != "Year"]

# Convert only the identified numeric columns to numeric.
returns_data_numeric <- returns_data_renamed %>%
  mutate(across(all_of(numeric_cols), as.numeric))

# Remove any rows that became entirely NA after conversion across the numeric columns.
returns_data_clean <- returns_data_numeric %>%
  filter(rowSums(is.na(select(., all_of(numeric_cols)))) < length(numeric_cols))

# Convert the cleaned data from wide format to long format.
returns_long <- returns_data_clean %>%
  pivot_longer(
    cols = -Year, # Exclude 'Year' column
    names_to = "Asset_Class", # New column to store the original column names
    values_to = "Annual_Return" # New column for return values
  ) %>%
  # Remove any rows where Annual_Return is NA.
  filter(!is.na(Annual_Return))

if (nrow(returns_long) == 0) {
  stop("No valid numeric data found for plotting after cleaning and transformation.
        Please check your Google Sheet for non-numeric entries in return columns or empty rows.")
}
message("Data transformation complete.")

# --- NEW: Explicitly Order Asset_Class Factor ---
message("Explicitly ordering asset classes on the x-axis...")

# Define the desired order of asset classes for the x-axis.
# This is the order you requested: D, E, F, G, H, B, C
desired_x_axis_order <- c(
  "3-mo US Treasury Bill",    # Column D
  "10-yr US Treasury Bond",   # Column E
  "Baa Corporate Bonds",      # Column F
  "Real Estate Price Only",   # Column G
  "Gold",                     # Column H
  "S&P 500 Total Return",     # Column B
  "US Small Cap Total Return" # Column C
)

# Reorder the Asset_Class factor levels
returns_long$Asset_Class <- factor(returns_long$Asset_Class, levels = desired_x_axis_order)

message("Asset classes ordered on x-axis as requested.")
message("Order of Asset Classes (from left to right in plot):")
print(levels(returns_long$Asset_Class))


# --- 6. Define Custom Colors for the Plot ---
plot_bg_color <- "#f5f7fa"
axis_line_color <- "#DC5318"
axis_font_color <- "#333f48"
kernel_density_color <- "#e8c547"

# Your desired color palette.
# IMPORTANT: Reorder this color vector to match the `desired_x_axis_order` exactly.
violin_fill_colors_ordered <- c(
  "3-mo US Treasury Bill" = "#928e56",      # Color for D
  "10-yr US Treasury Bond" = "#8fb8de",     # Color for E
  "Baa Corporate Bonds" = "#f7b076",        # Color for F
  "Real Estate Price Only" = "#DC5318",     # Color for G
  "Gold" = "#e8c547",                       # Color for H
  "S&P 500 Total Return" = "#2a6ca3",       # Color for B
  "US Small Cap Total Return" = "#4d3319"   # Color for C
)


# --- 7. Create the Violin Plot ---
message("Generating violin plot for all asset classes...")
p <- ggplot(returns_long, aes(x = Asset_Class, y = Annual_Return, fill = Asset_Class)) +
  geom_violin(trim = TRUE, alpha = 0.8, color = kernel_density_color, linewidth = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", alpha = 0.7) +
  # Use the explicitly ordered color vector for fill
  scale_fill_manual(values = violin_fill_colors_ordered, drop = FALSE) +
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentages
  labs(
    title = "Distribution of Annual Returns by Asset Class, 1928-2024",
    x = "Asset Class",
    y = "Annual Return" # Y-axis label updated as it's now formatted as percent
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = plot_bg_color, color = NA),
    panel.background = element_rect(fill = plot_bg_color, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = axis_line_color, linewidth = 0.5),
    axis.ticks = element_line(color = axis_line_color),
    axis.text.x = element_text(color = axis_font_color, size = 10, angle = 45, hjust = 1), # Angle labels for readability
    axis.title = element_text(color = axis_font_color, size = 12, face = "bold"),
    plot.title = element_text(color = axis_line_color, size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# --- 8. Display the Plot ---
print(p)
message("Plot generated successfully!")
