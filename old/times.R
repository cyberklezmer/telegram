# Load necessary packages
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(tibble)
library(xtable)

# Connect to MySQL database
con <- dbConnect(
  MySQL(),
  host = "bethel.utia.cas.cz",
  user = "jak",
  password = "jaknajaka",
  dbname = "telegram"
)

# Fetch the data from the database
messages_info <- dbGetQuery(con, "SELECT posted, src_posted FROM messages_info")

# Calculate the time difference in seconds
messages_info <- messages_info %>%
  mutate(time_diff = as.numeric(difftime(posted, src_posted, units = "secs")))

# Calculate descriptive statistics
descriptive_stats <- messages_info %>%
  summarize(
    mean_diff = mean(time_diff, na.rm = TRUE),
    sd_diff = sd(time_diff, na.rm = TRUE),
    median_diff = median(time_diff, na.rm = TRUE),
    min_diff = min(time_diff, na.rm = TRUE),
    max_diff = max(time_diff, na.rm = TRUE)
  )

# Save descriptive statistics to disk
write.csv(descriptive_stats, "descriptive_statistics.csv", row.names = FALSE)

# Plot the histogram of the decimal logarithm of time differences
hist_plot <- ggplot(messages_info, aes(x = log10(time_diff))) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram of Log10 Time Differences", x = "Log10 Time Difference (seconds)", y = "Frequency") +
  theme_minimal()

# Save the histogram to disk
ggsave("log10_time_difference_histogram.png", plot = hist_plot)

# Print descriptive statistics to console
print(descriptive_stats)

dbDisconnect(con)
