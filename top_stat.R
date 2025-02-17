# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats

library(ppcor)

source("defs.R")

con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)



write.csv(catalogue_data, paste0(output_folder,islandid, "_catalogue_data.csv"), row.names = FALSE)

top_channels <- get_top_channels(con, catalogue_data,10)


# Save to CSV file
output_file_csv <- file.path(output_folder, paste0(islandid, "_top_channels_by_reach.csv"))
write.csv(top_channels, output_file_csv, row.names = FALSE)


# Load necessary packages

# Ensure reactions_per_msg_user is formatted to 3 decimal places
top_channels_fmt <- top_channels %>%
  mutate(account = substr(account,1,20),
         rpmu = round(reactions_per_msg_user, 2),
         msgs_K = round(msg_count / 1000, 2),
         reach_M = round(reach_own / 1000000,2)
         ) %>%
  rename(subscr = subscribers,
         accpt = accepted
         )
# Select required columns
selected_columns <- c("account", "subscr", "msgs_K", "reach_M",
                      "accpt", "sent", "rpmu")

# Create LaTeX table
latex_table <- xtable(top_channels_fmt[selected_columns])

# Save to disk
output_file <- paste0(output_folder,islandid,"_top_channels_table.tex")
sink(output_file)

# Load necessary package
# Save to a .tex file
print(latex_table, include.rownames = FALSE)
sink()

# Convert data into long format for ggplot2 compatibility
plot_data <- top_channels %>%
  mutate(
    username = str_trunc(username, 25)
  ) %>%
  dplyr::select(username, accepted, sent) %>%
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
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1, vjust = 1), # Larger, skewed x-axis labels
    axis.text.y = element_text(size = 12),                                  # Larger y-axis labels
    axis.title.x = element_text(size = 16),                                 # Larger x-axis title
    axis.title.y = element_text(size = 16),                                 # Larger y-axis title
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
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold"),  # Larger title
    legend.text = element_text(size = 14),  # Larger legend text
    legend.title = element_text(size = 16)  # Larger legend title
  ) +
  geom_text(aes(label = paste0(username, ": ", scales::number(reach_own / 1000000, accuracy = 1), "M")),
            position = position_stack(vjust = 0.5),
            check_overlap = TRUE,
            size = 6)  # Increase text size inside the plot

# Uložení grafu 'sum_members'

output_file_png <- file.path(output_folder, paste0(islandid,"_reach_summary.png"))

ggsave(output_file_png, plot = plot_sum_members, width = 10, height = 6)





# messages_data <- get_messages(con)  







# Close the database connectionint
dbDisconnect(con)



