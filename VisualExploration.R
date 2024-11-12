# Load necessary libraries
library(haven)

# Load the dataset (assuming the dataset is named 'support')
support <- read_dta("support.dta")

# Select variables for the project without using the pipe operator
selected_vars <- support[c("age", "totcst", "num_co", "edu")]

# Set up the layout for multiple plots (3 rows, 2 columns) and add space for better visualization
par(mfrow = c(2, 2), mar = c(6, 5, 5, 2), oma = c(2, 2, 4, 2)) # Reduced outer margin for the overall plot

# Create histograms for each numeric variable with a box around each plot
boxplot.col <- "black"  # Color for the border box
border_width <- 2        # Width of the border

# Add a box to each plot with custom color and width
add_border <- function() {
  box(which = "figure", lty = "solid", col = boxplot.col, lwd = border_width)
}

# Generate histograms and add a border to each plot
hist(selected_vars$age, main = "Histogram of Age", xlab = "Age", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

hist(selected_vars$totcst, main = "Histogram of Total Hospital Cost", xlab = "Total Cost", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

hist(selected_vars$num_co, main = "Histogram of Number of Comorbidities", xlab = "Number of Comorbidities", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

hist(selected_vars$edu, main = "Histogram of Years of Education", xlab = "Years of Education", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

# Add a meta title for all plots with increased size and reduced margin
mtext("Histograms of Selected Variables", outer = TRUE, cex = 2, font = 2, line = 1)

# Save the figure as a PNG file
dev.copy(png, file = "Results/histograms_selected_vars.png", width = 800, height = 800)
dev.off()

# Reset layout to default
par(mfrow = c(1, 1))

# =============================================================================

# Load the dataset (assuming the dataset is named 'support')
support <- read_dta("support.dta")

# Convert categorical variables to factors with meaningful labels
support$sex <- factor(support$sex, levels = c(1, 2), labels = c("Male", "Female"))
support$dzclass <- factor(support$dzclass, levels = c(1, 2, 3, 4), labels = c("Class I", "Class II", "Class III", "Class IV"))

# Set up the layout for multiple plots (1 row, 2 columns) and save as PNG
png(filename = "barplots_selected_vars_bw.png", width = 1200, height = 600)

par(mfrow = c(1, 2), mar = c(5, 5, 5, 2), oma = c(2, 2, 4, 2))

# Bar Plot for Gender of the Patient
barplot(table(support$sex), main = "Bar Plot of Gender", xlab = "Gender", ylab = "Frequency", col = "grey", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
box(which = "figure", lty = "solid", col = "black", lwd = 2)

# Bar Plot for Disease Classification
barplot(table(support$dzclass), main = "Bar Plot of Disease Classification", xlab = "Disease Class", ylab = "Frequency", col = "grey", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
box(which = "figure", lty = "solid", col = "black", lwd = 2)

# Add a meta title for all plots
mtext("Bar Plots of Selected Categorical Variables", outer = TRUE, cex = 2, font = 2, line = 1)

# Save and close the PNG device
dev.off()

# Reset layout to default
par(mfrow = c(1, 1))


# =============================================================================
# Remove rows with missing values for the selected variables to ensure plots are not empty
numerical_vars_clean <- na.omit(numerical_vars)

# Set up the layout for multiple boxplots (2 rows, 2 columns) and add space for better visualization
par(mfrow = c(2, 2), mar = c(6, 5, 5, 2), oma = c(2, 2, 4, 2)) # Adjust margins for better readability

# Create boxplots for each numerical variable with a box around each plot
boxplot.col <- "black"  # Color for the border box
border_width <- 2        # Width of the border

# Add a box to each plot with custom color and width
add_border <- function() {
  box(which = "figure", lty = "solid", col = boxplot.col, lwd = border_width)
}

# Generate boxplots and add a border to each plot
boxplot(numerical_vars_clean$age, main = "Boxplot of Age", ylab = "Age", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

boxplot(numerical_vars_clean$totcst, main = "Boxplot of Total Hospital Cost", ylab = "Total Cost", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

boxplot(numerical_vars_clean$num_co, main = "Boxplot of Number of Comorbidities", ylab = "Number of Comorbidities", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

boxplot(numerical_vars_clean$edu, main = "Boxplot of Years of Education", ylab = "Years of Education", col = "grey", border = "black", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
add_border()

# Add a meta title for all plots with increased size and reduced margin
mtext("Boxplots of Selected Variables", outer = TRUE, cex = 2, font = 2, line = 1)

# Save the figure as a PNG file
dev.copy(png, file = "boxplots_selected_vars.png", width = 1200, height = 800)
dev.off()

# Reset layout to default
par(mfrow = c(1, 1))



