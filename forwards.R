# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats
num_channels <- 25


# treshold for displaying of a line in graph
forward_treshold <- 5

source("defs.R")

con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)


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



# Close the database connectionint
dbDisconnect(con)




