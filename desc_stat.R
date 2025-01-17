# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats
num_channels <- 25

start_date <- as.Date("2024-03-04")
end_date <- as.Date("2024-09-29")

source("defs.R")

con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)

output_file_csv <- paste0(output_folder, islandid,"_catalogue_data.csv")

catalogue_data <- catalogue_data %>%
  mutate(
    accepted = forwarded_to / msg_count,
    sent = forwarded_from / msg_count,
    reactions_per_msessge = reaction_count / msg_count,
    reactions_per_user = reaction_count / subscribers,
    reactions_per_msg_user = reaction_count / subscribers / msg_count
  )


write.csv(catalogue_data, output_file_csv, row.names = FALSE)

top_channels <- catalogue_data %>%
  arrange(desc(reach_own)) %>%
  head(num_channels)


# Save to CSV file
output_file_csv <- file.path(output_folder, paste0(islandid, "_top_channels_by_reach.csv"))
write.csv(top_channels, output_file_csv, row.names = FALSE)

# Convert data into long format for ggplot2 compatibility
plot_data <- top_channels %>%
  mutate(
    username = str_trunc(username, 25),    # Trim usernames to 25 characters
    accepted = forwarded_to / msg_count,
    sent = forwarded_from / msg_count
  ) %>%
  select(username, accepted, sent) %>%
  pivot_longer(cols = c("accepted", "sent"), names_to = "type", values_to = "value") %>%
  mutate(value = if_else(type == "accepted", -value, value)) # accepted as negative

# Create the plot with larger labels
plot_dist <- ggplot(plot_data, aes(x = username, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, expand = c(0, 0)) +
  labs(
    x = "Username",
    y = "Ratio",
    fill = "Type",
    title = "Sent (above) and Accepted (below) Ratios by Username"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1), # Larger, skewed x-axis labels
    axis.text.y = element_text(size = 12),                                  # Larger y-axis labels
    legend.position = "top"
  )

# Create the plot


output_file_png <- file.path(output_folder, paste0(islandid,"_reach_dist.png"))

ggsave(output_file_png, plot = plot_dist, width = 10, height = 6)


others_channels <- catalogue_data %>%
  filter(!username %in% top_channels$username) %>%
  summarise(username = "Other", reach_own = sum(reach_own))

reach_summary <- bind_rows(top_channels, others_channels)

output_file_csv <- file.path(output_folder, paste0(islandid,"_reach_summary.csv"))

write.csv(reach_summary, output_file_csv, row.names = FALSE)

# Vykreslení koláčového grafu pro 'sum_members'
plot_sum_members <- ggplot(reach_summary, aes(x = "", y = reach_own, fill = username)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Channels by Reach",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(username, ": ", scales::number(reach_own / 1000000, accuracy = 1),"M")),
            position = position_stack(vjust = 0.5), check_overlap = TRUE)

# Uložení grafu 'sum_members'

output_file_png <- file.path(output_folder, paste0(islandid,"_reach_summary.png"))

ggsave(output_file_png, plot = plot_sum_members, width = 10, height = 6)





messages_data <-  dbGetQuery(con,"SELECT channel_id, src_channel_id, posted FROM messages" )

messages_data <- messages_data %>%
  mutate(posted_date = as.Date(posted)) %>%
  filter(posted_date >= start_date & posted_date <= end_date)




# Assuming messages_data is already loaded and has the structure provided
# Add a column to classify messages as forwarded or not
# Join top_channels with messages_data to include username
# Join top_channels with messages_data to include username
messages_data <- messages_data %>%
  left_join(select(top_channels, channel_id, username), by = "channel_id")

# Ensure `username` follows the order in `top_channels`
messages_data <- messages_data %>%
  mutate(username = factor(username, levels = top_channels$username))

# Add a column to classify messages as forwarded or not
messages_data <- messages_data %>%
  mutate(forwarded = ifelse(!is.na(src_channel_id), "Forwarded", "Not Forwarded"))

# Filter messages to include only the top channels
filtered_messages <- messages_data %>%
  filter(channel_id %in% top_channels$channel_id)

# Extract the date part from the `posted` column
filtered_messages <- filtered_messages %>%
  mutate(posted_date = as.Date(posted))

# Aggregate messages by username, date, and forwarding status
agg_messages <- filtered_messages %>%
  group_by(username, posted_date, forwarded) %>%
  summarise(count = n(), .groups = "drop")

# Create the grid plot using facet_wrap with ordered username
p <- ggplot(agg_messages, aes(x = posted_date, y = count, fill = forwarded)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ username, ncol = 5, scales = "free_x") +  # Use ordered username for facets
  labs(title = "Message Time Series for Top Channels",
       x = "Date",
       y = "Number of Messages",
       fill = "Message Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a PDF
ggsave(paste0(output_folder, islandid, "_top_channels_messages.pdf"), plot = p, width = 15, height = 10)


weekly_messages <- messages_data %>%
  mutate(week = as.Date(cut(as.Date(posted), breaks = "week"))) %>%
  group_by(username, week) %>%
  summarise(total_messages = n(), .groups = "drop")  # Sum forwarded and non-forwarded

correlation_data <- weekly_messages %>%
  pivot_wider(names_from = username, values_from = total_messages, values_fill = 0)

write.csv(correlation_data, file.path(output_folder, paste0(islandid, "_correlation_data.csv")), row.names = TRUE)


# Step 4: Compute the correlation matrix
correlation_matrix <- cor(correlation_data[ , -1], use = "pairwise.complete.obs")  # Exclude 'week'

# Step 5: Save the correlation matrix as a CSV
write.csv(correlation_matrix, file.path(output_folder, paste0(islandid, "_correlation_matrix.csv")), row.names = TRUE)

# Step 6: Create a heatmap

correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("Channel1", "Channel2", "Correlation")

heatmap_plot <- ggplot(correlation_df, aes(x = Channel1, y = Channel2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Weekly Messages",
       x = "Channel",
       y = "Channel",
       fill = "Correlation")

# Save the heatmap
ggsave(file.path(output_folder, paste0(islandid, "_correlation_heatmap.pdf")), plot = heatmap_plot, width = 10, height = 8)




# Close the database connectionint
dbDisconnect(con)



