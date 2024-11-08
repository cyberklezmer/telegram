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
messages_info <- dbGetQuery(con, "SELECT 
    mi.channel_id,
    mi.msg_id,
    mi.posted,
    mi.src_channel_id,
    mi.src_msg_id,
    mi.src_posted,
    ci.lang
FROM 
    messages_info mi
JOIN 
    channels_info ci
ON 
    mi.channel_id = ci.channel_id;
")

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

# Plot histograms of the decimal logarithm of time differences by language
hist_plot <- ggplot(messages_info, aes(x = log10(time_diff))) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  scale_x_continuous(name = "Log10 Time Difference (seconds)") +
  facet_wrap(~ lang, scales = "free") +
  labs(title = "Histogram of Log10 Time Differences by Language", 
       y = "Frequency") +
  theme_minimal()

# Save the histogram to disk
ggsave("log10_time_difference_histogram_by_language.png", plot = hist_plot, width = 12, height = 8)

# Print descriptive statistics to console
print(descriptive_stats)

# Save workspace
save.image("workspace.RData")

# Disconnect from the database
dbDisconnect(con)
