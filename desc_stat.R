library(DBI)
library(RMySQL)
library(dplyr)
library(stringr)
library(xtable)
library(knitr)
library(ggplot2)
library(tidyr)
library(stringr)
library(igraph)
library(ggraph)
library(tibble)
# Define output folder for CSV and PDF
output_folder <- "out/"

# Connect to MySQL database
con <- dbConnect(
  MySQL(),
  host = "bethel.utia.cas.cz",
  user = "jak",
  password = "jaknajaka",
  dbname = "telegram_full"
)

# Query the database for relevant data
catalogue_data <- dbGetQuery(con, "SELECT channel_id, username, CONVERT(name USING ASCII) as name, subscribers, catalogue_count,
             msg_count, forwarded_from, forwarded_to, lang FROM channels_info WHERE (source = 'slerka')")

 
# Clean and convert subscribers column
catalogue_data <- catalogue_data %>%
  mutate(subscribers = if_else(
    str_detect(subscribers, "K"),
    as.numeric(str_replace(subscribers, "K", "")) * 1000,
    if_else(
      str_detect(subscribers, "M"),as.numeric(str_replace(subscribers, "M", "")) * 1000000, as.numeric(subscribers))
  ))

catalogue_data <- catalogue_data %>%
  mutate(reach_own = subscribers * msg_count)

output_file_csv <- file.path(output_folder, "catalogue_data.csv")

write.csv(catalogue_data, output_file_csv, row.names = FALSE)

messages_data <-  dbGetQuery(con,"SELECT channel_id, src_channel_id FROM messages" )


catalogue_data <- catalogue_data %>%
  mutate(lang = if_else(lang != "CZECH" & lang != "SLOVAK","OTHER",lang))

    
type_summary <- catalogue_data %>%
  group_by(lang) %>%
  summarise(sum_reach = sum(reach_own),sum_subscribers=sum(subscribers),
            sum_msg_count = sum(msg_count)) 

   output_file_csv <- file.path(output_folder, "list_summary.csv")

   write.csv(type_summary, output_file_csv, row.names = FALSE)


for(i in (1:2)) 
{
  ns <- if_else(i==0,"OTHER",if_else(i==1, "CZECH","SLOVAK"))
  
  if(i==0)
  {
    c_data <- catalogue_data %>%
      filter(lang != "CZECH" & lang != "SLOVAK")
  }
  else
  {
     c_data <- catalogue_data  %>%
       filter(lang == ns)
  }

  top_channels <- c_data %>%
    arrange(desc(reach_own)) %>%
    head(20)
  
  
  
  # Step 1: Filter messages_data for interactions between top_channels
  top_channel_ids <- top_channels$channel_id
  
  interaction_data <- messages_data %>%
    filter(channel_id %in% top_channel_ids & src_channel_id %in% top_channel_ids) %>%
    count(src_channel_id, channel_id) %>%
    complete(src_channel_id = top_channel_ids, channel_id = top_channel_ids, fill = list(n = 0))
  
  interaction_data <- interaction_data %>%
    mutate(n = if_else(n<5,0,n))
  
  # Step 2: Create the adjacency matrix from the completed interaction data
  interaction_matrix <- interaction_data %>%
    pivot_wider(names_from = channel_id, values_from = n, values_fill = 0)
  
  # Ensure row names are set and convert to matrix
  rownames(interaction_matrix) <- interaction_matrix$src_channel_id
  interaction_matrix <- interaction_matrix %>%
    select(-src_channel_id) %>%
    as.matrix()
  
  # Step 3: Convert the matrix into an igraph object
  interaction_graph <- graph_from_adjacency_matrix(interaction_matrix, mode = "directed", weighted = TRUE)
  
  
  
  # Add vertex names (IDs) to the igraph object
  V(interaction_graph)$name <- str_trunc(top_channels$username, 20)

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
  
  output_path <- file.path(output_folder, paste0(ns,"_interaction_graph.png"))
  ggsave(output_path, plot = plot, width = 10, height = 8, dpi = 300)
  
  top_channels <- top_channels %>%
    mutate(
      accepted = forwarded_to / msg_count,
      sent = forwarded_from / msg_count
    )

    # Save to CSV file
  output_file_csv <- file.path(output_folder, paste0(ns, "_top_20_channels_by_reach.csv"))
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
   

  output_file_png <- file.path(output_folder, paste0(ns,"_reach_dist.png"))
  
  ggsave(output_file_png, plot = plot_dist, width = 10, height = 6)
  

  top_10_channels <- c_data %>% top_n(10, reach_own)%>%
                  arrange(desc(reach_own)) 
  others_channels <- c_data %>%
    filter(!username %in% top_10_channels$username) %>%
    summarise(username = "Other", reach_own = sum(reach_own))

  reach_summary <- bind_rows(top_10_channels, others_channels)
  
  output_file_csv <- file.path(output_folder, paste0(ns,"_reach_summary.csv"))
  
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
  
  output_file_png <- file.path(output_folder, paste0(ns,"_reach_summary.png"))
  
  ggsave(output_file_png, plot = plot_sum_members, width = 10, height = 6)

}




# Close the database connectionint
dbDisconnect(con)




