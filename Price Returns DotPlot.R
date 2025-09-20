library(ggplot2)

# Data for the companies (1-year price returns as of current date)
company_data <- data.frame(
  company = c("Coca Cola", "Veriz", "Disney", "META", "Caterpillar", "Salesforce", "Tesla", "Pfizer"),
  return_1year = c(22.3, 34.0, -11.8, 31.6, 2.8, 2.5, 60.2, -13.9) # Approximate percentages from search results
)

# Create a category for gain or loss
company_data$performance_category <- ifelse(company_data$return_1year >= 0, "Gain", "Loss")
company_data$performance_category <- factor(company_data$performance_category, levels = c("Loss", "Gain")) # Ensure order for color mapping

# Color scheme
colors <- c("Loss" = "#DC5318", "Gain" = "#333f48")

# Create the Cleveland dot plot with color based on performance and labels
ggplot(company_data, aes(x = return_1year, y = reorder(company, return_1year))) +
  geom_segment(aes(xend = 0, yend = company), color = "grey50") +
  geom_point(aes(color = performance_category), size = 4) +
  scale_color_manual(values = colors) + # Apply color scheme based on category
  scale_x_continuous(labels = function(x) paste0(x, "%")) + # Add percentage to x-axis
  geom_text(aes(label = paste0(round(return_1year, 1), "%")), # Add labels
            hjust = 0.5, vjust = 2.0, size = 3) + # Adjust label position (increased vjust)
  labs(title = "One-year price performance by company",
       x = "1-Year Price Return",
       y = "Company") +
  theme_minimal() +
  theme(legend.position = "bottom")