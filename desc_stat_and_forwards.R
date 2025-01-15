# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats
num_channels <- 25


# treshold for displaying of a line in graph
forward_treshold <- 5

source("defs.R")

con <- connect_db()


csqltxt <- paste0("SELECT channel_id, username, CONVERT(name USING ASCII) as name, subscribers, catalogue_count,
             msg_count, reaction_count, forwarded_from, forwarded_to, lang, sources FROM channels_info WHERE (sources LIKE '%T%' AND ", islandcondition, " )")

# Query the database for relevant data
catalogue_data <- dbGetQuery(con, csqltxt   )

 
catalogue_data <- catalogue_data %>%
  mutate(reach_own = subscribers * msg_count)

output_file_csv <- paste0(output_folder, islandid,"_catalogue_data.csv")

write.csv(catalogue_data, output_file_csv, row.names = FALSE)

messages_data <-  dbGetQuery(con,"SELECT channel_id, src_channel_id FROM messages" )

top_channels <- catalogue_data %>%
  arrange(desc(reach_own)) %>%
  head(num_channels)



# Step 1: Filter messages_data for interactions between top_channels
top_channel_ids <- top_channels$channel_id

interaction_data_base <- messages_data %>%
  filter(channel_id %in% top_channel_ids & src_channel_id %in% top_channel_ids) %>%
  count(src_channel_id, channel_id) %>%
  complete(src_channel_id = top_channel_ids, channel_id = top_channel_ids, fill = list(n = 0))

# Replace channel_id in interaction_data with corresponding username
interaction_data_base <- interaction_data_base %>%
  left_join(select(top_channels, channel_id, username), 
            by = c("src_channel_id" = "channel_id")) %>%
  rename(src_username = username) %>%
  left_join(select(top_channels, channel_id, username,msg_count), 
            by = c("channel_id" = "channel_id")) %>%
  rename(dest_username = username,
         dest_msg_count = msg_count) %>%
  select(src_username, dest_username, n, dest_msg_count)  # Keep only relevant columns

interaction_data <- interaction_data_base %>%
  select(src_username, dest_username, n)

interaction_data <- interaction_data %>%
  mutate(n = if_else(n<forward_treshold,0,n))

# Step 2: Create the adjacency matrix from the completed interaction data
interaction_matrix <- interaction_data %>%
  pivot_wider(names_from = dest_username, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "src_username") %>%
  as.matrix()

# Ensure row names are set and convert to matrix


output_file_matrix_csv <- file.path(output_folder, paste0( islandid,"_interaction_matrix", ".csv"))
write.csv(interaction_matrix, output_file_matrix_csv, row.names = TRUE)


# Step 3: Convert the matrix into an igraph object
interaction_graph <- graph_from_adjacency_matrix(interaction_matrix, mode = "directed", weighted = TRUE)

interaction_graph <- delete_vertices(interaction_graph, which(degree(interaction_graph) == 0))

# Print debugging info
cat("Number of vertices:", vcount(interaction_graph), "\n")
cat("Number of edges:", ecount(interaction_graph), "\n")

# Check if there are any edges with weights greater than 0
print(E(interaction_graph)$weight)

# Step 4: Simplify the plot and adjust width scaling
plot <- ggraph(interaction_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), 
                 arrow = arrow(length = unit(3, 'mm')), # Define arrow size for visibility
                 end_cap = circle(3, 'mm')) + # Adjust end cap for clarity
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 6) + # Increase label size to 6
  theme_void()  # Step 5: Save the plot as PNG

output_path <- file.path(output_folder, paste0(islandid,"_interaction_graph.png"))
ggsave(output_path, plot = plot, width = 10, height = 8, dpi = 300)


# relative graph
# Ensure that the necessary libraries are loaded

# Step 1: Create interaction_data_rel with the calculated column
interaction_data_rel <- interaction_data_base %>%
  mutate(percent_received = n / dest_msg_count) %>%
  select(src_username, dest_username, percent_received) %>%
  mutate(percent_received = if_else(percent_received < 0.001, 0, percent_received))

# Step 2: Transform interaction_data_rel into a matrix
interaction_matrix_rel <- interaction_data_rel %>%
  pivot_wider(names_from = dest_username, values_from = percent_received, values_fill = list(percent_received = 0)) %>%
  column_to_rownames(var = "src_username") %>%
  as.matrix()

# Check the result

# Ensure row names are set and convert to matrix


output_file_matrix_rel_csv <- file.path(output_folder, paste0( islandid,"_interaction_matrix_rel", ".csv"))
write.csv(interaction_matrix_rel, output_file_matrix_rel_csv, row.names = TRUE)


# Step 3: Convert the matrix into an igraph object
interaction_graph_rel <- graph_from_adjacency_matrix(interaction_matrix_rel, mode = "directed", weighted = TRUE)

interaction_graph_rel <- delete_vertices(interaction_graph_rel, which(degree(interaction_graph_rel) == 0))

# Print debugging info
cat("Number of vertices:", vcount(interaction_graph_rel), "\n")
cat("Number of edges:", ecount(interaction_graph_rel), "\n")

# Check if there are any edges with weights greater than 0
print(E(interaction_graph_rel)$weight)

# Step 4: Simplify the plot and adjust width scaling
plot_rel <- ggraph(interaction_graph_rel, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), 
                 arrow = arrow(length = unit(3, 'mm')), # Define arrow size for visibility
                 end_cap = circle(3, 'mm')) + # Adjust end cap for clarity
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 6) + # Increase label size to 6
  theme_void()  # Step 5: Save the plot as PNG

output_path_rel <- file.path(output_folder, paste0(islandid,"_interaction_graph_rel.png"))
ggsave(output_path_rel, plot = plot_rel, width = 10, height = 8, dpi = 300)

  
top_channels <- top_channels %>%
  mutate(
    accepted = forwarded_to / msg_count,
    sent = forwarded_from / msg_count,
    reacted = reaction_count / msg_count
  )

  # Save to CSV file
output_file_csv <- file.path(output_folder, paste0(islandid, "_top_20_channels_by_reach.csv"))
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


top_10_channels <- catalogue_data %>% top_n(num_channels, reach_own)%>%
                arrange(desc(reach_own)) 
others_channels <- catalogue_data %>%
  filter(!username %in% top_10_channels$username) %>%
  summarise(username = "Other", reach_own = sum(reach_own))

reach_summary <- bind_rows(top_10_channels, others_channels)

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






# Close the database connectionint
dbDisconnect(con)




