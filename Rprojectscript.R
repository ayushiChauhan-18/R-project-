
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")

library(ggplot2)
library(dplyr)
library(corrplot)

Step 1: Load the dataset
# The dataset can be downloaded and read directly, or imported if saved locally.
# Download from: https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
# Assuming the dataset is saved as 'winequality-red.csv' in the working directory:

wine<-read.csv("C:\\Users\\Ayushi\\OneDrive\\Desktop\\winequality-red.csv", sep = ";")

# Step 2: Explore the structure of the dataset
str(wine)
summary(wine)
View(wine)
# Check for missing values
sum(is.na(wine))

# Step 3: Distribution of variables using histograms
# Histograms to understand the distribution of each chemical feature and the target variable (quality)
for (col in names(wine)[-12]) {  # exclude 'quality' for now
  print(ggplot(wine, aes_string(col)) +
          geom_histogram(bins = 30, fill = "red", color = "black") +
          ggtitle(paste("Histogram of", col)) +
          theme_minimal())
}

# Visualize distribution of the target variable 'quality'
ggplot(wine, aes(x = quality)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Distribution of Wine Quality Ratings") +
  theme_minimal()

# Step 4: Identify and visualize outliers using boxplots
for (col in names(wine)[-12]) {  # exclude 'quality'
  print(ggplot(wine, aes_string(x = "'quality'", y = col)) +
          geom_boxplot(fill = "steelblue") +
          ggtitle(paste("Boxplot of", col, "vs Quality")) +
          theme_minimal())
}

# Step 5: Visualize relationships between variables
# Scatterplot to see the relationship between fixed acidity and pH
ggplot(wine, aes(x = fixed.acidity, y = pH)) +
  geom_point(color = "blue", alpha = 0.5) +
  ggtitle("Scatterplot of Fixed Acidity vs pH") +
  theme_minimal()

# Scatterplot of alcohol vs quality to see if alcohol content affects quality
ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Scatterplot of Alcohol vs Quality") +
  theme_minimal()

# Step 6: Correlation matrix and heatmap
correlation_matrix <- cor(wine[, -12])  # Exclude the 'quality' column for now
corrplot(correlation_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Optional: Check correlation of features with 'quality'
correlation_with_quality <- cor(wine)[, 'quality']
print(correlation_with_quality)

# Heatmap to visualize the correlation between variables
corrplot(cor(wine), method = "number")
