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

# Define and execute the SQL query
query <- "
SELECT
    t.channel_id,
    t.msg_id,
    t.src_channel_id,
    t.src_msg_id,
    t.orig_channel_id,
    t.orig_msg_id,
    t.chain_length,
    t.posted,
    t.created_at,
    t.channel_lang,
    t.src_channel_lang,
    COUNT(m.src_msg_id) AS src_msg_count
FROM
    tmp_messages_with_lang t
LEFT JOIN
    messages m
ON
    t.channel_id = m.src_channel_id AND t.msg_id = m.src_msg_id
GROUP BY
    t.channel_id,
    t.msg_id;"
data <- dbGetQuery(con, query)

# Prepare data for Poisson regression
# Convert channel_lang and channel_id to factors (dummy variables)
data <- data %>%
  mutate(channel_lang = factor(channel_lang),
         channel_id = factor(channel_id))

# Compute the averages and variances of src_msg_count for individual languages
stats_src_msg_count <- data %>%
  group_by(channel_lang) %>%
  summarise(
    avg_src_msg_count = mean(src_msg_count, na.rm = TRUE),
    var_src_msg_count = var(src_msg_count, na.rm = TRUE)
  )
write.csv(data, "~/tmp/msgs.csv", row.names = FALSE)
# Print the averages and variances
print(stats_src_msg_count)

# Create histograms of src_msg_count for individual languages
histograms <- ggplot(data, aes(x = src_msg_count, fill = channel_lang)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = 'identity') +
  facet_wrap(~ channel_lang, scales = 'free_y') +
  labs(title = "Histograms of src_msg_count by Channel Language",
       x = "Source Message Count",
       y = "Frequency") +
  theme_minimal()

# Display the histograms
print(histograms)

# Save the histograms to a file
ggsave("histograms_src_msg_count_by_language.png", plot = histograms, width = 10, height = 8)

# Disconnect from the database
dbDisconnect(con)
