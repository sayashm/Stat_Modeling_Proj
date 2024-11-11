# Load necessary libraries
library(haven)
library(kableExtra)
library(webshot)
library(gridExtra)
library(grid)

# Load the dataset (assuming the dataset is named 'support')
support <- read_dta("support.dta")

# Select variables for the project without using the pipe operator
selected_vars <- support[c("age", "sex", "dzclass", "num_co", "edu", "totcst", "hospdead")]

# Create a data frame for summary statistics in the desired format
summary_df <- data.frame(
  Description = c(
    "Age of the patient",
    "Gender of the patient",
    "Disease classification",
    "Number of comorbidities",
    "Years of education",
    "Total hospital cost",
    "In-hospital death"
  ),
  Median = sapply(selected_vars, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
  Mean = sapply(selected_vars, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
  Third_Quarter = sapply(selected_vars, function(x) if(is.numeric(x)) quantile(x, 0.75, na.rm = TRUE) else NA),
  Max = sapply(selected_vars, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
  NAs = sapply(selected_vars, function(x) sum(is.na(x)))
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
webshot("summary_selected_vars.html", "summary_selected_vars.png", vwidth = 1000, vheight = 250)
