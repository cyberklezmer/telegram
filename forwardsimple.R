# Load necessary packages
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(tibble)
library(xtable)

# Function to calculate skewness
skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sum(((x - mean_x) / sd_x)^3, na.rm = TRUE) * n / ((n - 1) * (n - 2))
}

# Function to calculate kurtosis
kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sum(((x - mean_x) / sd_x)^4, na.rm = TRUE) * n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

# Connect to MySQL database
con <- dbConnect(
  MySQL(),
  host = "bethel.utia.cas.cz",
  user = "jak",
  password = "jaknajaka",
  dbname = "telegram"
)

# Define and execute the SQL query
query <- "
SELECT 
    m1.channel_id, 
    m1.msg_id, 
    m1.src_channel_id, 
    m1.src_msg_id,
    m1.orig_channel_id, 
    m1.orig_msg_id, 
    m1.chain_length, 
    m1.posted, 
    m1.created_at,
    COUNT(m2.channel_id) AS count_matching_records
FROM 
    messages m1
LEFT JOIN 
    messages m2 ON m2.src_channel_id = m1.channel_id 
               AND m2.src_msg_id = m1.msg_id
GROUP BY 
    m1.channel_id, 
    m1.msg_id;
"
data <- dbGetQuery(con, query)

# Compute descriptive statistics
descriptive_stats <- data %>%
  summarize(
    mean = mean(count_matching_records, na.rm = TRUE),
    median = median(count_matching_records, na.rm = TRUE),
    sd = sd(count_matching_records, na.rm = TRUE),
    var = var(count_matching_records, na.rm = TRUE),
    min = min(count_matching_records, na.rm = TRUE),
    max = max(count_matching_records, na.rm = TRUE)
  )

print(descriptive_stats)

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "descriptive_statistics.csv", row.names = FALSE)

# Calculate histogram data
hist_data <- data %>%
  count(count_matching_records) %>%
  rename(frequency = n)

# Save histogram frequencies to CSV
write.csv(hist_data, "histogram_frequencies.csv", row.names = FALSE)

# Plot histogram
histogram_plot <- ggplot(data, aes(x = count_matching_records)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(
    title = "Histogram of count_matching_records",
    x = "Count of Matching Records",
    y = "Frequency"
  ) +
  theme_minimal()

# Save plot to file
ggsave("histogram_plot.png", plot = histogram_plot)

# Display the plot
print(histogram_plot)

# Calculate PMF for data
data_pmf <- hist_data %>%
  mutate(probability = frequency / sum(frequency)) %>%
  filter(count_matching_records >= 2)

# Calculate Poisson PMF
lambda <- mean(data$count_matching_records, na.rm = TRUE)
poisson_pmf <- data.frame(
  count_matching_records = 2:max(data$count_matching_records, na.rm = TRUE),
  probability = dpois(2:max(data$count_matching_records, na.rm = TRUE), lambda)
)

# Save PMFs to CSV
write.csv(data_pmf, "data_pmf.csv", row.names = FALSE)
write.csv(poisson_pmf, "poisson_pmf.csv", row.names = FALSE)

# Merge data PMF and Poisson PMF for comparison
comparison_pmf <- data_pmf %>%
  full_join(poisson_pmf, by = "count_matching_records", suffix = c("_data", "_poisson")) %>%
  replace_na(list(probability_data = 0, probability_poisson = 0))

# Reshape data for plotting
comparison_pmf_long <- comparison_pmf %>%
  pivot_longer(cols = starts_with("probability"), 
               names_to = "type", 
               names_prefix = "probability_", 
               values_to = "probability")

# Plot PMFs using bar plots
pmf_plot <- ggplot(comparison_pmf_long, aes(x = count_matching_records, y = probability, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Data PMF", "Poisson PMF")) +
  labs(
    title = "PMF of count_matching_records and Poisson Distribution",
    x = "Count of Matching Records",
    y = "Probability",
    fill = "Type"
  ) +
  theme_minimal()

# Save PMF plot to file
ggsave("pmf_plot.png", plot = pmf_plot, width = 10, height = 6)

# Display the PMF plot
print(pmf_plot)

# Save R workspace image
# save.image(file = "workspace.RData")

# Disconnect from the database
dbDisconnect(con)
