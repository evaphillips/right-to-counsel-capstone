# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
dataset <- read.csv('A:\\files\\tract.csv')

# Viewing the first few rows of the dataset
head(dataset)

# Example of data manipulation: Grouping and summarizing data
# Here, we calculate mean filing rate and sum of filings by year and county
data_summary <- dataset %>%
  group_by(year, county) %>%
  summarize(mean_filing_rate = mean(filing_rate, na.rm = TRUE),
            total_filings = sum(filings, na.rm = TRUE))

# Viewing the summary
head(data_summary)

# Example of data visualization: Plotting filings over the years for a specific county
# Replace 'County' with the actual county name you're interested in
ggplot(data=subset(dataset, county == 'Wayne County'), aes(x=year, y=filings)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title='Eviction Filings Over the Years', x='Year', y='Number of Filings')

# replace 'path_to_your_dataset' with the actual path to your dataset.
