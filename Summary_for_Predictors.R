# Load necessary libraries
library(haven)
library(kableExtra)
library(webshot)

# Load the dataset (assuming the dataset is named 'support')
support <- read_dta("support.dta")

# Select numerical variables for the summary statistics
numerical_vars <- support[c("age", "num_co", "edu", "totcst")]

# Create a data frame for summary statistics in the desired format
summary_df <- data.frame(
  Description = c(
    "Age of the patient",
    "Number of comorbidities",
    "Years of education",
    "Total hospital cost"
  ),
  Median = sapply(numerical_vars, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
  Mean = sapply(numerical_vars, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
  Third_Quarter = sapply(numerical_vars, function(x) if(is.numeric(x)) quantile(x, 0.75, na.rm = TRUE) else NA),
  Max = sapply(numerical_vars, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
  NAs = sapply(numerical_vars, function(x) sum(is.na(x)))
)

# Format the summary table using kableExtra for better appearance
formatted_table <- kable(summary_df, format = "html", col.names = c("Description", "Median", "Mean", "3rd Quarter", "Max", "NAs"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

# Save the formatted table as an HTML file
save_kable(formatted_table, "summary_selected_vars.html")

# Use Webshot to convert HTML table to an image
# Make sure webshot and phantomjs are installed
webshot::install_phantomjs()
webshot("summary_selected_vars.html", "summary_selected_vars.png", vwidth = 1000, vheight = 150)

# =============================================================================

# Frequency count for categorical variables
support$sex <- factor(support$sex, levels = c(1, 2), labels = c("Male", "Female"))
support$dzclass <- factor(support$dzclass, levels = c(1, 2, 3, 4), labels = c("Class I", "Class II", "Class III", "Class IV"))

sex_freq <- table(support$sex)
dzclass_freq <- table(support$dzclass)

# Create a data frame for frequency counts without the 'Variable' column
freq_df <- data.frame(
  Category = c(names(sex_freq), names(dzclass_freq)),
  Frequency = c(as.numeric(sex_freq), as.numeric(dzclass_freq))
)

# Format the frequency table using kableExtra for better appearance
formatted_freq_table <- kable(freq_df, format = "html", col.names = c("Category", "Frequency"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3") %>%
  group_rows("Gender of the patient", 1, length(sex_freq)) %>%
  group_rows("Disease classification", length(sex_freq) + 1, length(sex_freq) + length(dzclass_freq))

# Save the formatted frequency table as an HTML file
save_kable(formatted_freq_table, "frequency_selected_vars.html")

# Use Webshot to convert HTML table to an image, adjusting the height to eliminate extra white space
webshot("frequency_selected_vars.html", "frequency_selected_vars.png", vwidth = 800, vheight = 250)
