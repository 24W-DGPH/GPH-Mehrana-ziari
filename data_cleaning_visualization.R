# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggcorrplot)

# Load dataset with correct column types
medical_data <- read_csv("medical_costs.csv", col_types = cols(
  Id = col_integer(),
  age = col_double(),
  sex = col_character(),
  bmi = col_double(),
  children = col_integer(),
  smoker = col_character(),
  region = col_character(),
  charges = col_double()
))

# Data Cleaning and Preprocessing
medical_data <- medical_data %>%
  distinct() %>%
  mutate(
    age = suppressWarnings(as.numeric(age)),
    bmi = suppressWarnings(as.numeric(bmi)),
    children = suppressWarnings(as.integer(children)),
    charges = suppressWarnings(as.numeric(charges)),
    sex = factor(sex, levels = c("male", "female")),
    smoker = factor(smoker, levels = c("no", "yes"), labels = c("Non-Smoker", "Smoker")),
    region = factor(region),
    age_group = cut(
      age, 
      breaks = c(0, 18, 35, 50, 65, 100),
      labels = c("Child", "Young Adult", "Middle-Aged", "Senior", "Elderly"),
      right = FALSE
    ),
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 24.9 ~ "Normal",
      bmi >= 25 & bmi < 29.9 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ "Unknown"
    )
  )

# Save cleaned data back to CSV
write_csv(medical_data, "medical_costs_cleaned.csv")

# Correlation Heatmap (for numeric columns)
numeric_columns <- select(medical_data, age, bmi, children, charges)

if (nrow(numeric_columns) > 1) {
  cor_matrix <- cor(numeric_columns, use = "complete.obs")
  
  heatmap_plot <- ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap")
  ggsave("heatmap.png", plot = heatmap_plot, width = 8, height = 6, dpi = 300)
} else {
  warning("Not enough numeric data for correlation heatmap.")
}

# Age Distribution Histogram
age_plot <- ggplot(medical_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

ggsave("age_distribution.png", plot = age_plot, width = 8, height = 6, dpi = 300)

# BMI Category Bar Plot
bmi_plot <- ggplot(medical_data, aes(x = bmi_category, fill = bmi_category)) +
  geom_bar() +
  labs(title = "BMI Category Distribution", x = "BMI Category", y = "Count") +
  theme_minimal()

ggsave("bmi_distribution.png", plot = bmi_plot, width = 8, height = 6, dpi = 300)

# Charges by Smoking Status
charges_plot <- ggplot(medical_data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = "Medical Charges by Smoking Status", x = "Smoking Status", y = "Charges") +
  theme_minimal()

ggsave("charges_by_smoker.png", plot = charges_plot, width = 8, height = 6, dpi = 300)

# NEW: Medical Charges by Region (Boxplot)
charges_region_plot <- ggplot(medical_data, aes(x = region, y = charges, fill = region)) +
  geom_boxplot() +
  labs(title = "Medical Charges by Region", x = "Region", y = "Charges") +
  theme_minimal()

ggsave("charges_by_region.png", plot = charges_region_plot, width = 8, height = 6, dpi = 300)

# NEW: Count of People in Each Region (Bar Chart)
region_count_plot <- ggplot(medical_data, aes(x = region, fill = region)) +
  geom_bar() +
  labs(title = "Number of People in Each Region", x = "Region", y = "Count") +
  theme_minimal()

ggsave("region_count.png", plot = region_count_plot, width = 8, height = 6, dpi = 300)
