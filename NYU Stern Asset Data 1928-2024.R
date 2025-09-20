# ---
# Title: Wealth Accumulation Line Graphs (Final Corrected Import)
# Author: Gemini
# Date: 2024-07-02
# Description: This script uses a robust method to read data from a Google
#              Sheet by ignoring headers and manually assigning column names,
#              guaranteeing a correct import and plot labeling.
# ---

# 1. SETUP: LOAD NECESSARY LIBRARIES
# --------------------------------------------------------------------------
# If you don't have these packages installed, you can install them by running:
# install.packages(c("googlesheets4", "ggplot2", "dplyr", "tidyr", "scales", "readr", "ggtext"))

library(googlesheets4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(ggtext)

# 2. AUTHENTICATION & DATA IMPORT
# --------------------------------------------------------------------------
# This command tells R we are accessing a public sheet that does not require a login.
# IMPORTANT: The Google Sheet's sharing setting must be "Anyone with the link can view".
gs4_deauth()

# The URL of your Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1rzJmTcmxVKjqi7SgzjGuU4RxVzK9prHX7ojE3I-UZVQ/edit?usp=sharing"


# --- PLOT AESTHETICS ---
COLOR_BACKGROUND <- "#f5f7fa"
COLOR_AXIS_LINES_TITLE <- "#DC5318"
COLOR_AXIS_FONT <- "#333f48"

# --- Master Color Palette ---
# We define all specific colors here to ensure consistency.
MASTER_PALETTE <- c(
  "S&P 500" = "#2a6ca3",
  "Gold" = "#e8c547",
  "3-month T.Bill" = "#928e56",
  "US Small Cap" = "#3a3a3a",
  "US T.Bond" = "#8fb8de",
  "Baa Corp Bond" = "#f7b076",
  "Real Estate" = "#DC5318",
  "10-year T.Bond" = "#8fb8de" # Same as US T.Bond for consistency
)


# --- PART 1: REAL WEALTH (THE "SOURCE OF TRUTH" FOR ORDERING/COLORS) ---
# ==========================================================================

# 3. PREPARE REAL DATA (ROBUST IMPORT)
# --------------------------------------------------------------------------
cat("Reading Real Wealth data from 'Cumulative Real Growth' sheet...\n")
real_data_raw <- read_sheet(
  ss = sheet_url,
  sheet = "Cumulative Real Estate Adjusted",
  range = "A2:H98", # Read data cells from A2 to H98
  col_names = FALSE # Read without headers
)

# Manually assign the correct column names
names(real_data_raw) <- c(
  "Year", "S&P 500", "US Small Cap", "3-month T.Bill",
  "10-year T.Bond", "Baa Corp Bond", "Real Estate", "Gold"
)

real_data <- real_data_raw %>%
  mutate(across(everything(), ~parse_number(as.character(.)))) %>%
  filter(!is.na(Year))

# Create a starting point row
start_year <- min(real_data$Year) - 1
start_row_real <- as_tibble(t(c(start_year, rep(100, ncol(real_data) - 1))))
names(start_row_real) <- names(real_data)

# Combine and pivot data
real_plot_data <- bind_rows(start_row_real, real_data) %>%
  pivot_longer(cols = -Year, names_to = "Asset", values_to = "Wealth")

# --- Establish the CORRECT order and color palette from the real data ---
ordered_levels_master <- real_plot_data %>%
  filter(Year == max(Year)) %>%
  arrange(desc(Wealth)) %>%
  pull(Asset)

real_plot_data$Asset <- factor(real_plot_data$Asset, levels = ordered_levels_master)
color_palette_master <- setNames(MASTER_PALETTE[ordered_levels_master], ordered_levels_master)

# Create the summary box for the real plot
summary_box_real <- real_plot_data %>%
  filter(Year == max(Year)) %>%
  arrange(factor(Asset, levels = ordered_levels_master)) %>%
  mutate(
    label = paste0(
      "<span style='color:", color_palette_master[Asset], ";'><strong>",
      Asset, ":</strong> ", dollar(Wealth, accuracy = 1), "</span>"
    )
  )

annotation_text_real <- paste0(
  "<strong>Closing Wealth Amounts</strong><br>",
  paste(summary_box_real$label, collapse = "<br>")
)

# 4. CREATE REAL LINE GRAPH
# --------------------------------------------------------------------------
real_plot <- ggplot(real_plot_data, aes(x = Year, y = Wealth, color = Asset)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = "dotdash", color = "#DC5318", size = 0.8) +
  annotate(
    "richtext", x = min(real_plot_data$Year), y = Inf,
    label = annotation_text_real, hjust = 0, vjust = 1,
    size = 4.5, lineheight = 1.3, label.color = NA,
    fill = alpha(COLOR_BACKGROUND, 0.75)
  ) +
  scale_color_manual(values = color_palette_master) +
  scale_y_log10(labels = scales::dollar_format(), breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  scale_x_continuous(breaks = seq(1930, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(
    title = "Cumulative Growth of $100 (Real)",
    subtitle = "Wealth accumulation after adjusting for inflation (logarithmic scale)",
    x = NULL, y = "Real Investment Value ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.grid.major = element_line(color = "#d9d9d9", linetype = "solid"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 20, face = "bold"),
    plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 14, margin = margin(b = 15)),
    axis.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 12, face = "bold"),
    axis.text = element_text(color = COLOR_AXIS_FONT, size = 10),
    legend.position = "none"
  )

print(real_plot)
cat("\n--- Real Wealth Plot Generated ---\n\n")


# --- PART 2: NOMINAL WEALTH ACCUMULATION (WITHOUT INFLATION) ---
# ==========================================================================

# 5. PREPARE NOMINAL DATA
# --------------------------------------------------------------------------
cat("Reading Nominal Wealth data from 'Cumulative Nominal Growth' sheet...\n")
nominal_data_raw <- read_sheet(
  ss = sheet_url,
  sheet = "Cumulative Nominal Growth",
  range = "A2:H98",
  col_names = FALSE
)

names(nominal_data_raw) <- c(
  "Year", "S&P 500", "3-month T.Bill", "US T.Bond",
  "Baa Corp Bond", "Real Estate", "Gold", "US Small Cap"
)

nominal_data <- nominal_data_raw %>%
  mutate(across(everything(), ~parse_number(as.character(.)))) %>%
  filter(!is.na(Year))

start_row_nominal <- as_tibble(t(c(start_year, rep(100, ncol(nominal_data) - 1))))
names(start_row_nominal) <- names(nominal_data)

nominal_plot_data <- bind_rows(start_row_nominal, nominal_data) %>%
  pivot_longer(cols = -Year, names_to = "Asset", values_to = "Wealth")

# --- CORRECTED: Force the nominal data to use the same order as the real data ---
nominal_plot_data$Asset <- factor(nominal_plot_data$Asset, levels = ordered_levels_master)

# --- Create the summary box for the nominal plot ---
summary_box_nominal <- nominal_plot_data %>%
  filter(Year == max(Year)) %>%
  arrange(factor(Asset, levels = ordered_levels_master)) %>%
  mutate(
    label = paste0(
      "<span style='color:", color_palette_master[Asset], ";'><strong>",
      Asset, ":</strong> ", dollar(Wealth, accuracy = 1), "</span>"
    )
  )

annotation_text_nominal <- paste0(
  "<strong>Closing Wealth Amounts</strong><br>",
  paste(summary_box_nominal$label, collapse = "<br>")
)

# 6. CREATE NOMINAL LINE GRAPH
# --------------------------------------------------------------------------
nominal_plot <- ggplot(nominal_plot_data, aes(x = Year, y = Wealth, color = Asset)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = "dotdash", color = "#DC5318", size = 0.8) +
  annotate(
    "richtext", x = min(nominal_plot_data$Year), y = Inf,
    label = annotation_text_nominal, hjust = 0, vjust = 1,
    size = 4.5, lineheight = 1.3, label.color = NA,
    fill = alpha(COLOR_BACKGROUND, 0.75)
  ) +
  scale_color_manual(values = color_palette_master) +
  scale_y_log10(labels = scales::dollar_format(), breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
  scale_x_continuous(breaks = seq(1930, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(
    title = "Cumulative Growth of $100 (Nominal)",
    subtitle = "Wealth accumulation without adjusting for inflation (logarithmic scale)",
    x = NULL, y = "Investment Value ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.background = element_rect(fill = COLOR_BACKGROUND, color = NA),
    panel.grid.major = element_line(color = "#d9d9d9", linetype = "solid"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 20, face = "bold"),
    plot.subtitle = element_text(color = COLOR_AXIS_FONT, size = 14, margin = margin(b = 15)),
    axis.title = element_text(color = COLOR_AXIS_LINES_TITLE, size = 12, face = "bold"),
    axis.text = element_text(color = COLOR_AXIS_FONT, size = 10),
    legend.position = "none"
  )

print(nominal_plot)
cat("\n--- Nominal Wealth Plot Generated ---\n")
