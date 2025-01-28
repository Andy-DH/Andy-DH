# Load the libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(showtext)

# Enable showtext
showtext_auto()

# Set the font to "DejaVu Sans"
font_add("DejaVu Sans", regular = "DejaVuSans.ttf", bold = "DejaVuSans-Bold.ttf", italic = "DejaVuSans-Oblique.ttf", bolditalic = "DejaVuSans-BoldOblique.ttf")

# Read and clean the data
breast_cancer_data <- read.csv("breast-cancer-dataset.csv")

# Inspect the unique values in the Tumor.Size..cm. column
print(unique(breast_cancer_data$Tumor.Size..cm.))

# Clean the data
clean_data <- breast_cancer_data %>%
  mutate(
    Menopause = factor(Menopause, levels = c(0, 1), labels = c("Post", "Pre")),
    Breast = factor(Breast),
    Metastasis = factor(Metastasis, levels = c(0, 1), labels = c("No", "Yes")),
    History = factor(History, levels = c(0, 1), labels = c("No", "Yes")),
    Diagnosis = factor(`Diagnosis.Result`, levels = c("Benign", "Malignant")),
    Age = as.numeric(Age),  # Convert Age to numeric
    Tumor.Size..cm. = as.numeric(Tumor.Size..cm.)  # Convert Tumor.Size..cm. to numeric
  ) %>%
  filter(!is.na(Age) & Age != "#") %>%
  filter(!is.na(Tumor.Size..cm.) & Tumor.Size..cm. != "#")

# Save the cleaned data to a CSV file
write.csv(clean_data, "cleaned_breast_cancer_data.csv", row.names = FALSE)

# Basic statistics
summary_stats <- summary(clean_data)

# Save the summary statistics to a text file
write(capture.output(summary_stats), file = "summary_stats.txt")

# Age distribution by diagnosis
age_diagnosis_plot <- ggplot(clean_data, aes(x = Age, fill = Diagnosis)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  theme_minimal() +
  labs(title = "Age Distribution by Diagnosis",
       x = "Age",
       y = "Count") +
  theme(text = element_text(family = "DejaVu Sans"))

# Save the age distribution by diagnosis plot using png device
png("age_diagnosis_plot.png", width = 800, height = 600)
print(age_diagnosis_plot)
dev.off()

# Tumor size by diagnosis
tumor_diagnosis_plot <- ggplot(clean_data, aes(x = Tumor.Size..cm., fill = Diagnosis)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Tumor Size Distribution by Diagnosis",
       x = "Tumor Size (cm)",
       y = "Count") +
  theme(text = element_text(family = "DejaVu Sans"))

# Save the tumor size by diagnosis plot using png device
png("tumor_diagnosis_plot.png", width = 800, height = 600)
print(tumor_diagnosis_plot)
dev.off()

# History correlation with diagnosis
history_diagnosis <- table(clean_data$History, clean_data$Diagnosis)

# Save the history correlation table to a CSV file
write.csv(as.data.frame.matrix(history_diagnosis), "history_diagnosis.csv", row.names = TRUE)
